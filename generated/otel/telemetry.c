
// CNS Forge OTEL Telemetry Instrumentation
#include <opentelemetry/api.h>

static auto tracer = opentelemetry::trace::Provider::GetTracerProvider()
    ->GetTracer("cns-forge", "1.0.0");

void instrument_bitactor_tick(signal_t* sig) {
    auto span = tracer->StartSpan("bitactor_tick");
    span->SetAttribute("signal.type", sig->type);
    span->SetAttribute("signal.timestamp", sig->timestamp);
    
    // Existing tick logic here
    
    span->End();
}
