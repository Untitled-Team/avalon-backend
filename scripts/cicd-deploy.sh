#! /bin/bash
# exit script when any command ran here returns with non-zero exit code
set -e

LATEST_GITHUB_TAG=$(git describe --abbrev=0)
LATEST_TAG=${LATEST_GITHUB_TAG:1}

# We must export it so it's available for envsubst
export LATEST_TAG=$LATEST_TAG

# since the only way for envsubst to work on files is using input/output redirection,
#  it's not possible to do in-place substitution, so we need to save the output to another file
#  and overwrite the original with that one.
envsubst <./kube/avalon-game-deployment.yml >./kube/avalon-game-deployment.yml.out
mv ./kube/avalon-game-deployment.yml.out ./kube/avalon-game-deployment.yml

echo "$KUBERNETES_CLUSTER_CERTIFICATE" | base64 --decode > cert.crt

./kubectl \
  --kubeconfig=/dev/null \
  --server=$KUBERNETES_SERVER \
  --certificate-authority=cert.crt \
  --token=$KUBERNETES_TOKEN \
  apply -f ./kube/