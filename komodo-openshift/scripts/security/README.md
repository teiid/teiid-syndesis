# Directory contents

* root
A generated root Certificate Authority (CA) for use with user-generated client certificates. This should only be used for testing purposes as not appropriate for production. Users should submit their client CSR to a recognised CA.

* intermediate
A generated intermediate Certificate Authority who is trusted by the root CA. Client certificates should be generated against this CA using the create-certificate.sh script. This will not only generate the certificate but import it into a JKS keystore for use with JBoss/EAP/Wildfly.

* In both cases the password for the private keys is raleigh
