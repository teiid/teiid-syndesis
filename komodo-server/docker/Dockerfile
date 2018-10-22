#######################################################################
# This is from fabric8 plugin uses for WildFly-Swarm                 #
#######################################################################

FROM fabric8/s2i-java:2.1
ENV AB_JOLOKIA_OFF=true AB_OFF=true JAVA_APP_DIR=/deployments
EXPOSE 8080 8778 9779
COPY maven /deployments/
