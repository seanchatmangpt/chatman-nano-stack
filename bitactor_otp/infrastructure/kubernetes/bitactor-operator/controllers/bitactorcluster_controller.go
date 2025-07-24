package controllers

import (
	"context"
	"fmt"
	"time"

	"github.com/go-logr/logr"
	appsv1 "k8s.io/api/apps/v1"
	corev1 "k8s.io/api/core/v1"
	"k8s.io/apimachinery/pkg/api/errors"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
	"k8s.io/apimachinery/pkg/types"
	"k8s.io/apimachinery/pkg/util/intstr"
	"k8s.io/client-go/tools/record"
	ctrl "sigs.k8s.io/controller-runtime"
	"sigs.k8s.io/controller-runtime/pkg/client"
	"sigs.k8s.io/controller-runtime/pkg/controller/controllerutil"

	uhftv1 "github.com/bitactor/operator/api/v1"
)

const (
	// Annotations
	cpuManagerPolicyAnnotation = "kubelet.kubernetes.io/cpu-manager-policy"
	cpuAffinityAnnotation      = "bitactor.io/cpu-affinity"
	numaNodeAnnotation        = "bitactor.io/numa-node"
	
	// Labels
	appLabel      = "app"
	clusterLabel  = "cluster"
	workloadLabel = "workload"
	
	// Finalizers
	bitactorFinalizer = "uhft.bitactor.io/finalizer"
)

// OperatorConfig holds operator configuration
type OperatorConfig struct {
	MaxRetries           int
	RetryBackoff         time.Duration
	PerformanceThreshold int64 // nanoseconds
	EnableAutoScaling    bool
	EnableChaosMonkey    bool
}

// BitActorClusterReconciler reconciles a BitActorCluster object
type BitActorClusterReconciler struct {
	client.Client
	Scheme   *runtime.Scheme
	Log      logr.Logger
	Recorder record.EventRecorder
	Config   OperatorConfig
}

// +kubebuilder:rbac:groups=uhft.bitactor.io,resources=bitactorclusters,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=uhft.bitactor.io,resources=bitactorclusters/status,verbs=get;update;patch
// +kubebuilder:rbac:groups=uhft.bitactor.io,resources=bitactorclusters/finalizers,verbs=update
// +kubebuilder:rbac:groups=apps,resources=statefulsets,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=core,resources=services,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=core,resources=configmaps,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=core,resources=secrets,verbs=get;list;watch
// +kubebuilder:rbac:groups=core,resources=pods,verbs=get;list;watch
// +kubebuilder:rbac:groups=core,resources=events,verbs=create;patch

func (r *BitActorClusterReconciler) Reconcile(ctx context.Context, req ctrl.Request) (ctrl.Result, error) {
	log := r.Log.WithValues("bitactorcluster", req.NamespacedName)

	// Fetch the BitActorCluster instance
	cluster := &uhftv1.BitActorCluster{}
	err := r.Get(ctx, req.NamespacedName, cluster)
	if err != nil {
		if errors.IsNotFound(err) {
			log.Info("BitActorCluster resource not found. Ignoring since object must be deleted")
			return ctrl.Result{}, nil
		}
		log.Error(err, "Failed to get BitActorCluster")
		return ctrl.Result{}, err
	}

	// Handle deletion
	if cluster.GetDeletionTimestamp() != nil {
		return r.reconcileDelete(ctx, cluster)
	}

	// Add finalizer if not present
	if !controllerutil.ContainsFinalizer(cluster, bitactorFinalizer) {
		controllerutil.AddFinalizer(cluster, bitactorFinalizer)
		if err := r.Update(ctx, cluster); err != nil {
			return ctrl.Result{}, err
		}
	}

	// Reconcile resources
	if err := r.reconcileResources(ctx, cluster); err != nil {
		r.Recorder.Event(cluster, corev1.EventTypeWarning, "ReconcileFailed", err.Error())
		return ctrl.Result{RequeueAfter: r.Config.RetryBackoff}, err
	}

	// Update status
	if err := r.updateStatus(ctx, cluster); err != nil {
		return ctrl.Result{}, err
	}

	// Check performance and scale if needed
	if r.Config.EnableAutoScaling {
		if err := r.checkAndScale(ctx, cluster); err != nil {
			log.Error(err, "Failed to check and scale")
		}
	}

	// Requeue for continuous monitoring
	return ctrl.Result{RequeueAfter: 30 * time.Second}, nil
}

func (r *BitActorClusterReconciler) reconcileResources(ctx context.Context, cluster *uhftv1.BitActorCluster) error {
	// Create headless service
	service := r.buildHeadlessService(cluster)
	if err := r.createOrUpdate(ctx, service, cluster); err != nil {
		return fmt.Errorf("failed to create/update service: %w", err)
	}

	// Create ConfigMap for configuration
	configMap := r.buildConfigMap(cluster)
	if err := r.createOrUpdate(ctx, configMap, cluster); err != nil {
		return fmt.Errorf("failed to create/update configmap: %w", err)
	}

	// Create StatefulSet
	statefulSet := r.buildStatefulSet(cluster)
	if err := r.createOrUpdate(ctx, statefulSet, cluster); err != nil {
		return fmt.Errorf("failed to create/update statefulset: %w", err)
	}

	r.Recorder.Event(cluster, corev1.EventTypeNormal, "Reconciled", "All resources reconciled successfully")
	return nil
}

func (r *BitActorClusterReconciler) buildStatefulSet(cluster *uhftv1.BitActorCluster) *appsv1.StatefulSet {
	labels := map[string]string{
		appLabel:      "bitactor",
		clusterLabel:  cluster.Name,
		workloadLabel: string(cluster.Spec.WorkloadType),
	}

	// Calculate resources based on workload type
	resources := r.calculateResources(cluster)

	// Build container with performance optimizations
	container := corev1.Container{
		Name:            "bitactor",
		Image:           fmt.Sprintf("bitactor/uhft:%s", cluster.Spec.Version),
		ImagePullPolicy: corev1.PullAlways,
		Env:             r.buildEnvironment(cluster),
		Ports:           r.buildPorts(cluster),
		Resources:       resources,
		SecurityContext: r.buildSecurityContext(cluster),
		VolumeMounts:    r.buildVolumeMounts(cluster),
		LivenessProbe:   r.buildProbe("/health", 8080, 30, 10),
		ReadinessProbe:  r.buildProbe("/ready", 8080, 10, 5),
	}

	// Pod template with performance settings
	podTemplate := corev1.PodTemplateSpec{
		ObjectMeta: metav1.ObjectMeta{
			Labels: labels,
			Annotations: map[string]string{
				"prometheus.io/scrape": "true",
				"prometheus.io/port":   "9090",
				"prometheus.io/path":   "/metrics",
			},
		},
		Spec: corev1.PodSpec{
			Affinity:        r.buildAffinity(cluster),
			NodeSelector:    r.buildNodeSelector(cluster),
			Tolerations:     r.buildTolerations(cluster),
			Containers:      []corev1.Container{container},
			Volumes:         r.buildVolumes(cluster),
			SecurityContext: &corev1.PodSecurityContext{},
		},
	}

	// Add performance-specific annotations
	if cluster.Spec.Performance.CPUPinning {
		podTemplate.Annotations[cpuManagerPolicyAnnotation] = "static"
		podTemplate.Annotations[cpuAffinityAnnotation] = "true"
	}
	if cluster.Spec.Performance.NumaNode != nil {
		podTemplate.Annotations[numaNodeAnnotation] = fmt.Sprintf("%d", *cluster.Spec.Performance.NumaNode)
	}

	replicas := int32(cluster.Spec.Replicas)
	
	return &appsv1.StatefulSet{
		ObjectMeta: metav1.ObjectMeta{
			Name:      cluster.Name,
			Namespace: cluster.Namespace,
			Labels:    labels,
		},
		Spec: appsv1.StatefulSetSpec{
			ServiceName: fmt.Sprintf("%s-headless", cluster.Name),
			Replicas:    &replicas,
			Selector: &metav1.LabelSelector{
				MatchLabels: labels,
			},
			Template: podTemplate,
			VolumeClaimTemplates: []corev1.PersistentVolumeClaim{
				{
					ObjectMeta: metav1.ObjectMeta{
						Name: "data",
					},
					Spec: corev1.PersistentVolumeClaimSpec{
						AccessModes: []corev1.PersistentVolumeAccessMode{
							corev1.ReadWriteOnce,
						},
						StorageClassName: &[]string{"fast-nvme"}[0],
						Resources: corev1.ResourceRequirements{
							Requests: corev1.ResourceList{
								corev1.ResourceStorage: resource.MustParse("100Gi"),
							},
						},
					},
				},
			},
			PodManagementPolicy: appsv1.ParallelPodManagement,
			UpdateStrategy: appsv1.StatefulSetUpdateStrategy{
				Type: appsv1.RollingUpdateStatefulSetStrategyType,
			},
		},
	}
}

func (r *BitActorClusterReconciler) calculateResources(cluster *uhftv1.BitActorCluster) corev1.ResourceRequirements {
	// Base resources by workload type
	resourceMap := map[uhftv1.WorkloadType]corev1.ResourceRequirements{
		uhftv1.MarketData: {
			Requests: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("16"),
				corev1.ResourceMemory: resource.MustParse("32Gi"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("16"),
				corev1.ResourceMemory: resource.MustParse("32Gi"),
			},
		},
		uhftv1.OrderEngine: {
			Requests: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("24"),
				corev1.ResourceMemory: resource.MustParse("96Gi"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("24"),
				corev1.ResourceMemory: resource.MustParse("96Gi"),
			},
		},
		uhftv1.RiskEngine: {
			Requests: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("32"),
				corev1.ResourceMemory: resource.MustParse("256Gi"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("32"),
				corev1.ResourceMemory: resource.MustParse("256Gi"),
			},
		},
		uhftv1.AlphaCalculator: {
			Requests: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("16"),
				corev1.ResourceMemory: resource.MustParse("64Gi"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("16"),
				corev1.ResourceMemory: resource.MustParse("64Gi"),
			},
		},
		uhftv1.ExecutionGateway: {
			Requests: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("8"),
				corev1.ResourceMemory: resource.MustParse("16Gi"),
			},
			Limits: corev1.ResourceList{
				corev1.ResourceCPU:    resource.MustParse("8"),
				corev1.ResourceMemory: resource.MustParse("16Gi"),
			},
		},
	}

	resources := resourceMap[cluster.Spec.WorkloadType]

	// Add hugepages if enabled
	if cluster.Spec.Performance.Hugepages {
		hugepagesKey := corev1.ResourceName("hugepages-2Mi")
		resources.Requests[hugepagesKey] = resource.MustParse("2Gi")
		resources.Limits[hugepagesKey] = resource.MustParse("2Gi")
	}

	// Add SR-IOV network devices if enabled
	if cluster.Spec.Networking.SrIov {
		sriovKey := corev1.ResourceName("intel.com/sriov_netdevice")
		resources.Requests[sriovKey] = resource.MustParse("1")
		resources.Limits[sriovKey] = resource.MustParse("1")
	}

	return resources
}

func (r *BitActorClusterReconciler) buildEnvironment(cluster *uhftv1.BitActorCluster) []corev1.EnvVar {
	env := []corev1.EnvVar{
		{
			Name:  "BITACTOR_ROLE",
			Value: string(cluster.Spec.WorkloadType),
		},
		{
			Name:  "BITACTOR_EXCHANGES",
			Value: fmt.Sprintf("%v", cluster.Spec.Exchanges),
		},
		{
			Name:  "BITACTOR_TARGET_LATENCY_NS",
			Value: fmt.Sprintf("%d", cluster.Spec.Performance.TargetLatencyNs),
		},
		{
			Name:  "BITACTOR_CPU_AFFINITY",
			Value: fmt.Sprintf("%t", cluster.Spec.Performance.CPUPinning),
		},
		{
			Name: "OTP_NODE_NAME",
			ValueFrom: &corev1.EnvVarSource{
				FieldRef: &corev1.ObjectFieldSelector{
					FieldPath: "metadata.name",
				},
			},
		},
		{
			Name: "OTP_COOKIE",
			ValueFrom: &corev1.EnvVarSource{
				SecretKeyRef: &corev1.SecretKeySelector{
					LocalObjectReference: corev1.LocalObjectReference{
						Name: "bitactor-erlang-cookie",
					},
					Key: "cookie",
				},
			},
		},
	}

	if cluster.Spec.Performance.NumaNode != nil {
		env = append(env, corev1.EnvVar{
			Name:  "BITACTOR_NUMA_NODE",
			Value: fmt.Sprintf("%d", *cluster.Spec.Performance.NumaNode),
		})
	}

	return env
}

func (r *BitActorClusterReconciler) buildSecurityContext(cluster *uhftv1.BitActorCluster) *corev1.SecurityContext {
	capabilities := []corev1.Capability{
		"NET_ADMIN",  // For network optimization
		"SYS_NICE",   // For CPU affinity
		"IPC_LOCK",   // For hugepages
	}

	if cluster.Spec.Networking.Dpdk {
		capabilities = append(capabilities, "SYS_RAWIO")
	}

	return &corev1.SecurityContext{
		Capabilities: &corev1.Capabilities{
			Add: capabilities,
		},
	}
}

func (r *BitActorClusterReconciler) updateStatus(ctx context.Context, cluster *uhftv1.BitActorCluster) error {
	// Get the StatefulSet
	statefulSet := &appsv1.StatefulSet{}
	err := r.Get(ctx, types.NamespacedName{
		Name:      cluster.Name,
		Namespace: cluster.Namespace,
	}, statefulSet)
	if err != nil {
		return err
	}

	// Update status
	cluster.Status.Phase = uhftv1.PhaseRunning
	cluster.Status.CurrentReplicas = statefulSet.Status.CurrentReplicas
	cluster.Status.ReadyReplicas = statefulSet.Status.ReadyReplicas

	// Get pods to collect metrics
	podList := &corev1.PodList{}
	if err := r.List(ctx, podList, client.InNamespace(cluster.Namespace),
		client.MatchingLabels{clusterLabel: cluster.Name}); err != nil {
		return err
	}

	// Collect metrics from pods (in real implementation, this would query Prometheus)
	if len(podList.Items) > 0 {
		// Simulated metrics for now
		cluster.Status.Metrics = uhftv1.PerformanceMetrics{
			P99LatencyNs: cluster.Spec.Performance.TargetLatencyNs - 100, // Under target
			Throughput:   1000000, // 1M messages/sec
			ErrorRate:    0.001,   // 0.1%
		}
	}

	cluster.Status.LastUpdated = metav1.Now()

	return r.Status().Update(ctx, cluster)
}

func (r *BitActorClusterReconciler) checkAndScale(ctx context.Context, cluster *uhftv1.BitActorCluster) error {
	// Auto-scaling logic based on performance metrics
	if cluster.Status.Metrics.P99LatencyNs > cluster.Spec.Performance.TargetLatencyNs {
		// Scale up if latency is above target
		newReplicas := cluster.Spec.Replicas + 1
		if newReplicas <= 20 { // Max replicas
			cluster.Spec.Replicas = newReplicas
			r.Recorder.Eventf(cluster, corev1.EventTypeNormal, "ScalingUp", 
				"Scaling up to %d replicas due to high latency (%dns > %dns)",
				newReplicas, cluster.Status.Metrics.P99LatencyNs, cluster.Spec.Performance.TargetLatencyNs)
			return r.Update(ctx, cluster)
		}
	} else if cluster.Status.Metrics.P99LatencyNs < (cluster.Spec.Performance.TargetLatencyNs / 2) {
		// Scale down if latency is well below target
		newReplicas := cluster.Spec.Replicas - 1
		if newReplicas >= 2 { // Min replicas
			cluster.Spec.Replicas = newReplicas
			r.Recorder.Eventf(cluster, corev1.EventTypeNormal, "ScalingDown", 
				"Scaling down to %d replicas due to low latency (%dns < %dns)",
				newReplicas, cluster.Status.Metrics.P99LatencyNs, cluster.Spec.Performance.TargetLatencyNs/2)
			return r.Update(ctx, cluster)
		}
	}

	return nil
}

func (r *BitActorClusterReconciler) reconcileDelete(ctx context.Context, cluster *uhftv1.BitActorCluster) (ctrl.Result, error) {
	r.Recorder.Event(cluster, corev1.EventTypeNormal, "Deleting", "Starting deletion of BitActorCluster")

	// Cleanup logic here (e.g., remove external resources)

	// Remove finalizer
	controllerutil.RemoveFinalizer(cluster, bitactorFinalizer)
	if err := r.Update(ctx, cluster); err != nil {
		return ctrl.Result{}, err
	}

	return ctrl.Result{}, nil
}

// Helper functions

func (r *BitActorClusterReconciler) createOrUpdate(ctx context.Context, obj client.Object, owner *uhftv1.BitActorCluster) error {
	if err := controllerutil.SetControllerReference(owner, obj, r.Scheme); err != nil {
		return err
	}

	return r.Patch(ctx, obj, client.Apply, client.ForceOwnership, client.FieldOwner("bitactor-operator"))
}

func (r *BitActorClusterReconciler) buildHeadlessService(cluster *uhftv1.BitActorCluster) *corev1.Service {
	return &corev1.Service{
		ObjectMeta: metav1.ObjectMeta{
			Name:      fmt.Sprintf("%s-headless", cluster.Name),
			Namespace: cluster.Namespace,
			Labels: map[string]string{
				appLabel:     "bitactor",
				clusterLabel: cluster.Name,
			},
		},
		Spec: corev1.ServiceSpec{
			ClusterIP: "None",
			Selector: map[string]string{
				appLabel:     "bitactor",
				clusterLabel: cluster.Name,
			},
			Ports: []corev1.ServicePort{
				{
					Name:       "epmd",
					Port:       4369,
					TargetPort: intstr.FromInt(4369),
				},
				{
					Name:       "bitactor",
					Port:       9100,
					TargetPort: intstr.FromInt(9100),
				},
			},
		},
	}
}

func (r *BitActorClusterReconciler) buildProbe(path string, port int32, initialDelay, period int32) *corev1.Probe {
	return &corev1.Probe{
		ProbeHandler: corev1.ProbeHandler{
			HTTPGet: &corev1.HTTPGetAction{
				Path: path,
				Port: intstr.FromInt(int(port)),
			},
		},
		InitialDelaySeconds: initialDelay,
		PeriodSeconds:       period,
		TimeoutSeconds:      3,
		SuccessThreshold:    1,
		FailureThreshold:    3,
	}
}

func (r *BitActorClusterReconciler) SetupWithManager(mgr ctrl.Manager) error {
	return ctrl.NewControllerManagedBy(mgr).
		For(&uhftv1.BitActorCluster{}).
		Owns(&appsv1.StatefulSet{}).
		Owns(&corev1.Service{}).
		Owns(&corev1.ConfigMap{}).
		Complete(r)
}