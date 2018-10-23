#######################################################################
# This is from fabric8 plugin uses for WildFly-Swarm                 #
#######################################################################

FROM registry.access.redhat.com/redhat-openjdk-18/openjdk18-openshift
ENV AB_JOLOKIA_OFF=true AB_OFF=true JAVA_APP_DIR=/deployments
EXPOSE 8080 8778 9779
COPY maven /deployments/
