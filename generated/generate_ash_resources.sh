#!/bin/bash

# Generated Ash resource creation script

echo "Creating domain MyApp.Assets..."
mix ash.gen.domain MyApp.Assets

echo "Creating domain MyApp.Controls..."
mix ash.gen.domain MyApp.Controls

echo "Creating domain MyApp.Threats..."
mix ash.gen.domain MyApp.Threats

echo "Creating resource MyApp.Assets.Asset..."
mix ash.gen.resource MyApp.Assets.Asset --domain MyApp.Assets --default-actions read,create,update,destroy --timestamps

echo "Creating resource MyApp.Assets.NetworkAsset..."
mix ash.gen.resource MyApp.Assets.NetworkAsset --domain MyApp.Assets --default-actions read,create,update,destroy --timestamps

echo "Creating resource MyApp.Threats.Threat..."
mix ash.gen.resource MyApp.Threats.Threat --domain MyApp.Threats --default-actions read,create,update,destroy --timestamps

echo "Creating resource MyApp.Threats.Vulnerability..."
mix ash.gen.resource MyApp.Threats.Vulnerability --domain MyApp.Threats --default-actions read,create,update,destroy --timestamps

echo "Creating resource MyApp.Controls.SecurityControl..."
mix ash.gen.resource MyApp.Controls.SecurityControl --domain MyApp.Controls --default-actions read,create,update,destroy --timestamps

echo "Creating resource MyApp.Controls.SecurityIncident..."
mix ash.gen.resource MyApp.Controls.SecurityIncident --domain MyApp.Controls --default-actions read,create,update,destroy --timestamps
