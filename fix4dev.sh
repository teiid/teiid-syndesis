#!/bin/bash
oc patch is syndesis-dv --type json -p='[{"op": "replace", "path": "/spec/tags/0/from/kind", "value": "DockerImage"},{"op": "replace", "path": "/spec/tags/0/from/name", "value": "docker.io/teiid/syndesis-dv:latest"}]'	

oc patch is syndesis-meta --type json -p='[{"op": "replace", "path": "/spec/tags/0/from/kind", "value": "DockerImage"},{"op": "replace", "path": "/spec/tags/0/from/name", "value": "docker.io/syndesis/syndesis-meta:latest"}]'

oc patch is syndesis-server --type json -p='[{"op": "replace", "path": "/spec/tags/0/from/kind", "value": "DockerImage"},{"op": "replace", "path": "/spec/tags/0/from/name", "value": "docker.io/syndesis/syndesis-server:latest"}]'

oc patch is syndesis-ui --type json -p='[{"op": "replace", "path": "/spec/tags/0/from/kind", "value": "DockerImage"},{"op": "replace", "path": "/spec/tags/0/from/name", "value": "docker.io/syndesis/syndesis-ui:latest"}]'

oc patch is syndesis-s2i --type json -p='[{"op": "replace", "path": "/spec/tags/0/from/kind", "value": "DockerImage"},{"op": "replace", "path": "/spec/tags/0/from/name", "value": "docker.io/syndesis/syndesis-s2i:latest"}]'
