/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest;

import java.io.IOException;
import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;

import org.komodo.metadata.DefaultMetadataInstance;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.spi.KException;
import org.komodo.spi.runtime.TeiidDataSource;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.kubernetes.client.LocalObjectReference;
import io.kubernetes.client.ModelServiceCatalogClient;
import io.kubernetes.client.ParametersFromSource;
import io.kubernetes.client.Secret;
import io.kubernetes.client.ServiceBinding;
import io.kubernetes.client.ServiceBindingList;
import io.kubernetes.client.ServiceInstance;
import io.kubernetes.client.ServiceInstanceList;

public class TeiidSwarmMetadataInstance extends DefaultMetadataInstance {

	private static final String OSURL = "https://openshift.default.svc";
	private static final String SC_VERSION = "v1beta1";
	private ModelServiceCatalogClient scClient;
	
    public TeiidSwarmMetadataInstance(TeiidConnectionProvider connectionProvider) {
		super(connectionProvider);
		this.scClient = new ModelServiceCatalogClient(OSURL, SC_VERSION);
	}
    
    public Admin adminX() throws AdminException {
    	return admin();
    }
    
    @Override
    public Collection<TeiidDataSource> getDataSources() throws KException {
        checkStarted();
        try {
        	Collection<String> dsNames = admin().getDataSourceNames();
        	String token = AuthHandlingFilter.threadOAuthCredentials.get().getToken();
        	System.out.println("Access token = "+token);
        	if (token != null) {
        		this.scClient.setAuthHeader(token);
        		
        		ServiceInstanceList serviceList = this.scClient.getServiceInstances(ApplicationProperties.getNamespace());
        		List<ServiceInstance> services = serviceList.getItems();
        		if( services != null && !services.isEmpty()) {
        			for (ServiceInstance svc:services) {
        				String name = svc.getMetadata().getName();
        				System.out.println("Service Name = "+name);
        				if (svc.getStatus().isReady()) {
        					if (!dsNames.contains(name)) {
        						ServiceBinding binding = getServiceBinding(svc);
        						if (binding == null) {
        							binding = this.scClient.createServiceBinding(svc);
        						}
        						Map<String, String> parameters = getParameters(svc);
        						Map<String, String> bindingSecrets = getBindingSecrets(binding);
        						bindingSecrets.putAll(parameters);
        						createDataSource(name, bindingSecrets);
        					}
        				} else {
        					System.out.println(name+":service not ready");
        				}
        			}
        		}
        	}
            return super.getDataSources();
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

	private Map<String, String> getBindingSecrets(ServiceBinding binding) throws IOException {
		Map<String, String> map = new TreeMap<>();
		if (binding != null) {
			String secretName = binding.getSpec().getSecretName();
			Secret secret = this.scClient.getSecret(ApplicationProperties.getNamespace(), secretName);
			map = secret.getData();
			Map<String, String> decodedMap = new TreeMap<>();
			for (Map.Entry<String, String> entry:map.entrySet()) {
				String key = entry.getKey();
				String encodedValue = entry.getValue();
				decodedMap.put(key, new String(Base64.getDecoder().decode(encodedValue)));
			}
			map = decodedMap;
		}
		return map;
	}

	private ServiceBinding getServiceBinding(ServiceInstance svc) throws IOException {
    	ServiceBindingList bindingList = this.scClient.getServiceBindings(ApplicationProperties.getNamespace());
		List<ServiceBinding> bindings = bindingList.getItems();
		if( bindings != null && !bindings.isEmpty()) {
			for (ServiceBinding sb : bindings) {
				LocalObjectReference ref = sb.getSpec().getServiceInstanceRef();
				if (ref.getName().equals(svc.getMetadata().getName())) {
					return sb;
				}
			}
		}
    	return null;
    }
    
    private Map<String, String> getParameters(ServiceInstance svc) throws IOException {
    	Map<String, String> map = new TreeMap<>();
    	
		// data source is there.
		ParametersFromSource parameters = null;
		for (int i = 0; i < svc.getSpec().getParametersFrom().size(); i++) {
			parameters = svc.getSpec().getParametersFrom().get(i);
			if(parameters.getSecretKeyRef().getKey().equalsIgnoreCase("parameters")) {
				break;
			}
		}
		if (parameters != null) {
			String secretName = parameters.getSecretKeyRef().getName();
			String key = parameters.getSecretKeyRef().getKey();
			Secret secret = this.scClient.getSecret(ApplicationProperties.getNamespace(),
					secretName);
			if (secret != null) {
				String json = secret.getData().get(key);
				map = new ObjectMapper().readerFor(Map.class).readValue(Base64.getDecoder().decode(json));					
			}
		} else {
			System.out.println(svc.getMetadata().getName()+":No Parameters Secret found");
		}  
		return map;
    }
    
	private void createDataSource(String name, Map<String, String> map) throws AdminException {
		if (map.get("POSTGRESQL_DATABASE") != null) {
			System.out.println("Creating the Datasource = "+name + " with properties ="+map);
			/*
			 {
			   "DATABASE_SERVICE_NAME":"postgresql",
			   "MEMORY_LIMIT":"512Mi",
			   "NAMESPACE":"openshift",
			   "database_name":"sampledb",
			   "password":"pass",
			   "username":"user",
			   "uri": "postgres://172.30.145.26:5432",
			   "POSTGRESQL_VERSION":"9.5",
			   "VOLUME_CAPACITY":"1Gi"
			}
			*/
			String driverName = null;
			Set<String> templateNames = admin().getDataSourceTemplateNames();
			System.out.println("template names:"+templateNames);
			for (String template : templateNames) {
				// TODO: there is null entering from above call from ,getDataSourceTemplateNames need to investigate why
				if (template != null && template.startsWith("postgresql")) {
					driverName = template;
					break;
				}
			}
			Properties props = new Properties();
			props.setProperty("connection-url", "jdbc:postgresql://" + map.get("DATABASE_SERVICE_NAME") + ":5432/"
					+ map.get("database_name"));
			props.setProperty("user-name", map.get("username"));
			props.setProperty("password", map.get("password"));
			admin().createDataSource(name, driverName, props);
		}
	}
    
}
