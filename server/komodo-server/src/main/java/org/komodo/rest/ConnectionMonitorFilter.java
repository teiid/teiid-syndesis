/*
 * Copyright Red Hat, Inc. and/or its affiliates
 * and other contributors as indicated by the @author tags and
 * the COPYRIGHT.txt file distributed with this work.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.rest;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.container.PreMatching;
import javax.ws.rs.ext.Provider;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.komodo.rest.AuthHandlingFilter.AuthToken;


@Provider
@PreMatching
public class ConnectionMonitorFilter implements ContainerRequestFilter {

    private static final Log LOGGER = LogFactory.getLog(ConnectionMonitorFilter.class);
    
    public static SyndesisConnectionMonitor monitor;
    
    public ConnectionMonitorFilter() {
    	super();
    	
    	LOGGER.info("  #####  ConnectionMonitorFilter()  Created ##### ");
    }
    

	@Override
	public void filter(ContainerRequestContext requestContext) throws IOException {
		boolean newMonitor = false;
		
		AuthToken token = canConnect();
		
		LOGGER.info("  *** ConnectionMonitorFilter.filter() *** ");
		if( token != null ) {
			if( monitor != null && !monitor.isConnected() ) {
				monitor.close();
				monitor = new SyndesisConnectionMonitor(null, token);
				newMonitor = true;
			} else if( monitor == null ) {
				monitor = new SyndesisConnectionMonitor(null, token);
				newMonitor = true;
			}
			if( newMonitor) {
				new MonitorThread(monitor).start();
			}
		} else {
			LOGGER.info("  *** ConnectionMonitorFilter.filter() *** CANNOT CONNECT. User Not LOGGED IN YET ");
		}
	
	}
	
	private AuthToken canConnect() {
		AuthToken token = AuthHandlingFilter.threadOAuthCredentials.get().getToken();
		if( token != null ) {
			
			String tokString = token.toString();
			if( tokString != null && tokString.length() > 0 ) {
				LOGGER.info("  *** ConnectionMonitorFilter.canConnect(TRUE) TOKEN Available = " + token );
				return token;
			}
		}
		LOGGER.info("  *** ConnectionMonitorFilter.canConnect(FALSE) TOKEN NOT AVAILABLE");
		return null;
	}
	
	class MonitorThread extends Thread {
		SyndesisConnectionMonitor monitor;
		
	    public MonitorThread(SyndesisConnectionMonitor monitor) {
	        super("monitorThread");
	        this.monitor = monitor;
	    }
	    
	    public void run() {
	    	LOGGER.info("  *** ConnectionMonitorFilter.MonitorThread.run() **** STARTED **** ");
	    	monitor.connect();
	    	
            try {
                sleep((int)(30 * 1000));
                LOGGER.info("  *** ConnectionMonitorFilter.MonitorThread.run() **** SLEPT FOR 30 seconds **** ");
                boolean isConnected = this.monitor.isConnected();
                if( !isConnected ) {
                	throw new InterruptedException("Web socket disconnected");
                }
            } catch (Exception e) {
            	LOGGER.info("  *** ConnectionMonitorFilter.MonitorThread.run() **** EXCEPTION **** \n" + e.getLocalizedMessage());
            	monitor.close();
            	monitor = null;
            }
        }
    }
}
