# CNS Forge 80/20 Makefile
# Leverages existing build patterns

CC = gcc
CFLAGS = -O3 -Wall -Wextra -std=c99
INCLUDES = -I. -I../src/cns -I../bitactor/include
LIBS = -lpthread -lm

# Targets
TARGETS = cns_forge_8020_demo test_cns_forge_8020 benchmark_cns_forge_8020

.PHONY: all clean test benchmark demo

all: $(TARGETS)

cns_forge_8020_demo: cns_forge_8020.c cns_forge_8020.h
	$(CC) $(CFLAGS) $(INCLUDES) -DCNS_FORGE_DEMO -o $@ cns_forge_8020.c $(LIBS)

test_cns_forge_8020: cns_forge_8020.c test_cns_forge_8020.c
	$(CC) $(CFLAGS) $(INCLUDES) -DCNS_FORGE_TEST -o $@ cns_forge_8020.c test_cns_forge_8020.c $(LIBS)

benchmark_cns_forge_8020: cns_forge_8020.c benchmark_cns_forge_8020.c
	$(CC) $(CFLAGS) $(INCLUDES) -DCNS_FORGE_BENCHMARK -o $@ cns_forge_8020.c benchmark_cns_forge_8020.c $(LIBS)

demo: cns_forge_8020_demo
	@echo "🚀 Running CNS Forge 80/20 Demo..."
	./cns_forge_8020_demo

test: test_cns_forge_8020
	@echo "🧪 Running CNS Forge 80/20 Tests..."
	./test_cns_forge_8020

benchmark: benchmark_cns_forge_8020
	@echo "📊 Running CNS Forge 80/20 Benchmarks..."
	./benchmark_cns_forge_8020

# Build Erlang components
erlang:
	@echo "🐿️ Building Erlang components..."
	cd ../bitactor_otp && rebar3 compile

# Deploy to Kubernetes
deploy:
	@echo "☸️ Deploying to Kubernetes..."
	kubectl apply -f cns-forge-8020-deployment.yaml

# Apply Terraform
terraform:
	@echo "🏗️ Applying Terraform configuration..."
	terraform init
	terraform apply -auto-approve

clean:
	rm -f $(TARGETS) *.o

install: all
	@echo "📦 Installing CNS Forge 80/20..."
	mkdir -p /usr/local/bin
	cp cns_forge_8020_demo /usr/local/bin/

.PHONY: erlang deploy terraform install
