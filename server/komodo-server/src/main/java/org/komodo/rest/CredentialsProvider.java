package org.komodo.rest;

import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;

public interface CredentialsProvider {
	
	OAuthCredentials getCredentials();

}
