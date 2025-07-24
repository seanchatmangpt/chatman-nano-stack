package v1

import (
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

// WorkloadType defines the type of BitActor workload
// +kubebuilder:validation:Enum=market-data;order-engine;risk-engine;alpha-calculator;execution-gateway
type WorkloadType string

const (
	MarketData       WorkloadType = "market-data"
	OrderEngine      WorkloadType = "order-engine"
	RiskEngine       WorkloadType = "risk-engine"
	AlphaCalculator  WorkloadType = "alpha-calculator"
	ExecutionGateway WorkloadType = "execution-gateway"
)

// Phase represents the current phase of the cluster
type Phase string

const (
	PhasePending   Phase = "Pending"
	PhaseRunning   Phase = "Running"
	PhaseFailed    Phase = "Failed"
	PhaseDeleting  Phase = "Deleting"
)

// PerformanceSpec defines performance requirements
type PerformanceSpec struct {
	// TargetLatencyNs is the target P99 latency in nanoseconds
	// +kubebuilder:validation:Minimum=100
	// +kubebuilder:validation:Maximum=1000000
	TargetLatencyNs int64 `json:"targetLatencyNs"`

	// CPUPinning enables CPU core pinning for consistent performance
	// +optional
	CPUPinning bool `json:"cpuPinning,omitempty"`

	// NumaNode specifies the NUMA node to pin to
	// +optional
	// +kubebuilder:validation:Minimum=0
	// +kubebuilder:validation:Maximum=8
	NumaNode *int32 `json:"numaNode,omitempty"`

	// Hugepages enables 2Mi hugepage allocation
	// +optional
	Hugepages bool `json:"hugepages,omitempty"`
}

// NetworkingSpec defines networking configuration
type NetworkingSpec struct {
	// SrIov enables SR-IOV for hardware-accelerated networking
	// +optional
	SrIov bool `json:"srIov,omitempty"`

	// Dpdk enables DPDK for userspace packet processing
	// +optional
	Dpdk bool `json:"dpdk,omitempty"`

	// Rdma enables RDMA for ultra-low latency networking
	// +optional
	Rdma bool `json:"rdma,omitempty"`
}

// BitActorClusterSpec defines the desired state of BitActorCluster
type BitActorClusterSpec struct {
	// Replicas is the desired number of BitActor instances
	// +kubebuilder:validation:Minimum=1
	// +kubebuilder:validation:Maximum=100
	Replicas int32 `json:"replicas"`

	// WorkloadType specifies the type of workload
	WorkloadType WorkloadType `json:"workloadType"`

	// Performance configuration
	Performance PerformanceSpec `json:"performance"`

	// Networking configuration
	// +optional
	Networking NetworkingSpec `json:"networking,omitempty"`

	// Exchanges to connect to
	// +optional
	Exchanges []string `json:"exchanges,omitempty"`

	// Version of BitActor to deploy
	// +optional
	// +kubebuilder:default="latest"
	Version string `json:"version,omitempty"`

	// Resources allows overriding default resource requirements
	// +optional
	Resources *ResourceRequirements `json:"resources,omitempty"`
}

// ResourceRequirements allows custom resource specification
type ResourceRequirements struct {
	// CPU cores required
	// +optional
	CPU string `json:"cpu,omitempty"`

	// Memory required
	// +optional
	Memory string `json:"memory,omitempty"`

	// GPU resources
	// +optional
	GPU string `json:"gpu,omitempty"`
}

// PerformanceMetrics contains real-time performance data
type PerformanceMetrics struct {
	// P99LatencyNs is the 99th percentile latency in nanoseconds
	P99LatencyNs int64 `json:"p99LatencyNs,omitempty"`

	// Throughput is messages processed per second
	Throughput int64 `json:"throughput,omitempty"`

	// ErrorRate is the percentage of failed operations
	ErrorRate float64 `json:"errorRate,omitempty"`

	// CPUUsage is the CPU utilization percentage
	CPUUsage float64 `json:"cpuUsage,omitempty"`

	// MemoryUsage is the memory usage in bytes
	MemoryUsageBytes int64 `json:"memoryUsageBytes,omitempty"`

	// NetworkLatencyUs is the network RTT in microseconds
	NetworkLatencyUs int64 `json:"networkLatencyUs,omitempty"`
}

// BitActorClusterStatus defines the observed state of BitActorCluster
type BitActorClusterStatus struct {
	// Phase represents the current phase of the cluster
	Phase Phase `json:"phase,omitempty"`

	// CurrentReplicas is the current number of replicas
	CurrentReplicas int32 `json:"currentReplicas,omitempty"`

	// ReadyReplicas is the number of ready replicas
	ReadyReplicas int32 `json:"readyReplicas,omitempty"`

	// Metrics contains current performance metrics
	Metrics PerformanceMetrics `json:"metrics,omitempty"`

	// LastUpdated is the last time the status was updated
	LastUpdated metav1.Time `json:"lastUpdated,omitempty"`

	// Conditions represent the latest available observations
	// +optional
	Conditions []metav1.Condition `json:"conditions,omitempty"`

	// Message provides additional status information
	// +optional
	Message string `json:"message,omitempty"`
}

// +kubebuilder:object:root=true
// +kubebuilder:subresource:status
// +kubebuilder:subresource:scale:specpath=.spec.replicas,statuspath=.status.currentReplicas,selectorpath=.status.selector
// +kubebuilder:printcolumn:name="Workload",type="string",JSONPath=".spec.workloadType",description="Type of workload"
// +kubebuilder:printcolumn:name="Replicas",type="integer",JSONPath=".spec.replicas",description="Desired replicas"
// +kubebuilder:printcolumn:name="Ready",type="integer",JSONPath=".status.readyReplicas",description="Ready replicas"
// +kubebuilder:printcolumn:name="P99 Latency",type="integer",JSONPath=".status.metrics.p99LatencyNs",description="P99 latency in nanoseconds"
// +kubebuilder:printcolumn:name="Phase",type="string",JSONPath=".status.phase",description="Current phase"
// +kubebuilder:printcolumn:name="Age",type="date",JSONPath=".metadata.creationTimestamp"

// BitActorCluster is the Schema for the bitactorclusters API
type BitActorCluster struct {
	metav1.TypeMeta   `json:",inline"`
	metav1.ObjectMeta `json:"metadata,omitempty"`

	Spec   BitActorClusterSpec   `json:"spec,omitempty"`
	Status BitActorClusterStatus `json:"status,omitempty"`
}

// +kubebuilder:object:root=true

// BitActorClusterList contains a list of BitActorCluster
type BitActorClusterList struct {
	metav1.TypeMeta `json:",inline"`
	metav1.ListMeta `json:"metadata,omitempty"`
	Items           []BitActorCluster `json:"items"`
}

func init() {
	SchemeBuilder.Register(&BitActorCluster{}, &BitActorClusterList{})
}