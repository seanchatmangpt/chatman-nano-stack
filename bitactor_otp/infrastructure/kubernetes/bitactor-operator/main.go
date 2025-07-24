package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"time"

	"go.uber.org/zap"
	"k8s.io/apimachinery/pkg/runtime"
	utilruntime "k8s.io/apimachinery/pkg/util/runtime"
	clientgoscheme "k8s.io/client-go/kubernetes/scheme"
	_ "k8s.io/client-go/plugin/pkg/client/auth"
	ctrl "sigs.k8s.io/controller-runtime"
	"sigs.k8s.io/controller-runtime/pkg/healthz"
	"sigs.k8s.io/controller-runtime/pkg/log/zap"
	metricsserver "sigs.k8s.io/controller-runtime/pkg/metrics/server"

	uhftv1 "github.com/bitactor/operator/api/v1"
	"github.com/bitactor/operator/controllers"
	// +kubebuilder:scaffold:imports
)

var (
	scheme   = runtime.NewScheme()
	setupLog = ctrl.Log.WithName("setup")
)

func init() {
	utilruntime.Must(clientgoscheme.AddToScheme(scheme))
	utilruntime.Must(uhftv1.AddToScheme(scheme))
	// +kubebuilder:scaffold:scheme
}

func main() {
	var metricsAddr string
	var enableLeaderElection bool
	var probeAddr string
	var maxConcurrentReconciles int
	
	flag.StringVar(&metricsAddr, "metrics-bind-address", ":8080", "The address the metric endpoint binds to.")
	flag.StringVar(&probeAddr, "health-probe-bind-address", ":8081", "The address the probe endpoint binds to.")
	flag.BoolVar(&enableLeaderElection, "leader-elect", false,
		"Enable leader election for controller manager. "+
			"Enabling this will ensure there is only one active controller manager.")
	flag.IntVar(&maxConcurrentReconciles, "max-concurrent-reconciles", 3,
		"Maximum number of concurrent reconciles for BitActorCluster controller")
	
	opts := zap.Options{
		Development: true,
		TimeEncoder: zapcore.ISO8601TimeEncoder,
	}
	opts.BindFlags(flag.CommandLine)
	flag.Parse()

	ctrl.SetLogger(zap.New(zap.UseFlagOptions(&opts)))

	// Create manager with performance optimizations
	mgr, err := ctrl.NewManager(ctrl.GetConfigOrDie(), ctrl.Options{
		Scheme: scheme,
		Metrics: metricsserver.Options{
			BindAddress: metricsAddr,
		},
		HealthProbeBindAddress: probeAddr,
		LeaderElection:         enableLeaderElection,
		LeaderElectionID:       "bitactor-operator-leader",
		// Performance tuning
		SyncPeriod: &[]time.Duration{30 * time.Second}[0],
		// Limit concurrent reconciles for stability
		MaxConcurrentReconciles: maxConcurrentReconciles,
	})
	if err != nil {
		setupLog.Error(err, "unable to start manager")
		os.Exit(1)
	}

	// Setup BitActorCluster controller with performance monitoring
	if err = (&controllers.BitActorClusterReconciler{
		Client:   mgr.GetClient(),
		Scheme:   mgr.GetScheme(),
		Recorder: mgr.GetEventRecorderFor("bitactor-controller"),
		Config: controllers.OperatorConfig{
			MaxRetries:           5,
			RetryBackoff:         time.Second,
			PerformanceThreshold: 1000, // 1 microsecond in nanoseconds
			EnableAutoScaling:    true,
			EnableChaosMonkey:    false, // Disabled by default
		},
	}).SetupWithManager(mgr); err != nil {
		setupLog.Error(err, "unable to create controller", "controller", "BitActorCluster")
		os.Exit(1)
	}

	// Setup webhooks for admission control
	if err = (&uhftv1.BitActorCluster{}).SetupWebhookWithManager(mgr); err != nil {
		setupLog.Error(err, "unable to create webhook", "webhook", "BitActorCluster")
		os.Exit(1)
	}

	// Health and readiness probes
	if err := mgr.AddHealthzCheck("healthz", healthz.Ping); err != nil {
		setupLog.Error(err, "unable to set up health check")
		os.Exit(1)
	}
	if err := mgr.AddReadyzCheck("readyz", healthz.Ping); err != nil {
		setupLog.Error(err, "unable to set up ready check")
		os.Exit(1)
	}

	// Start performance monitor
	go startPerformanceMonitor(mgr.GetClient())

	setupLog.Info("starting BitActor operator")
	if err := mgr.Start(ctrl.SetupSignalHandler()); err != nil {
		setupLog.Error(err, "problem running manager")
		os.Exit(1)
	}
}

// Performance monitoring goroutine
func startPerformanceMonitor(client client.Client) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	for {
		select {
		case <-ticker.C:
			// List all BitActorClusters
			var clusters uhftv1.BitActorClusterList
			if err := client.List(context.Background(), &clusters); err != nil {
				setupLog.Error(err, "failed to list BitActorClusters")
				continue
			}

			// Check performance metrics for each cluster
			for _, cluster := range clusters.Items {
				if cluster.Status.Metrics.P99LatencyNs > cluster.Spec.Performance.TargetLatencyNs {
					setupLog.Warn("Performance degradation detected",
						"cluster", cluster.Name,
						"namespace", cluster.Namespace,
						"p99_latency_ns", cluster.Status.Metrics.P99LatencyNs,
						"target_ns", cluster.Spec.Performance.TargetLatencyNs)
					
					// Trigger scaling or optimization
					// This would be handled by the reconciler
				}
			}
		}
	}
}