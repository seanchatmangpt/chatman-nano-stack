#!/bin/bash

# Generated Ash resource creation script

echo "Creating domain Cybersec.Assets..."
mix ash.gen.domain Cybersec.Assets

echo "Creating domain Cybersec.Controls..."
mix ash.gen.domain Cybersec.Controls

echo "Creating domain Cybersec.Threats..."
mix ash.gen.domain Cybersec.Threats

echo "Creating resource Cybersec.Assets.Asset..."
mix ash.gen.resource Cybersec.Assets.Asset --domain Cybersec.Assets --default-actions read,create,update,destroy --timestamps

echo "Creating resource Cybersec.Assets.NetworkAsset..."
mix ash.gen.resource Cybersec.Assets.NetworkAsset --domain Cybersec.Assets --default-actions read,create,update,destroy --timestamps

echo "Creating resource Cybersec.Threats.Threat..."
mix ash.gen.resource Cybersec.Threats.Threat --domain Cybersec.Threats --default-actions read,create,update,destroy --timestamps

echo "Creating resource Cybersec.Threats.Vulnerability..."
mix ash.gen.resource Cybersec.Threats.Vulnerability --domain Cybersec.Threats --default-actions read,create,update,destroy --timestamps

echo "Creating resource Cybersec.Controls.SecurityControl..."
mix ash.gen.resource Cybersec.Controls.SecurityControl --domain Cybersec.Controls --default-actions read,create,update,destroy --timestamps

echo "Creating resource Cybersec.Controls.SecurityIncident..."
mix ash.gen.resource Cybersec.Controls.SecurityIncident --domain Cybersec.Controls --default-actions read,create,update,destroy --timestamps
