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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import org.komodo.metadata.DefaultMetadataInstance;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.spi.KException;
import org.komodo.spi.repository.ApplicationProperties;
import org.komodo.spi.runtime.ServiceCatalogDataSource;
import org.komodo.utils.KLog;
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
	private enum SourceType {postgresql, mysql, mongodb, salesforce};
	
    public TeiidSwarmMetadataInstance(TeiidConnectionProvider connectionProvider) {
		super(connectionProvider);
		this.scClient = new ModelServiceCatalogClient(OSURL, SC_VERSION);
	}
    
	@Override
	public Set<ServiceCatalogDataSource> getServiceCatalogSources() throws KException {
    	String token = AuthHandlingFilter.threadOAuthCredentials.get().getToken();
    	this.scClient.setAuthHeader(token);
    	Set<ServiceCatalogDataSource> sources = new HashSet<>();
    	try {
			ServiceInstanceList serviceList = this.scClient.getServiceInstances(ApplicationProperties.getNamespace());
			List<ServiceInstance> services = serviceList.getItems();
			if( services != null && !services.isEmpty()) {
				for (ServiceInstance svc : services) {
					if (svc.getStatus().isReady()) {
						Map<String, String> parameters = getParameters(svc);
						SourceType type = matchType(svc, parameters);
						if (type != null) {
							ServiceCatalogDataSourceImpl scd = new ServiceCatalogDataSourceImpl();
							scd.setName(svc.getMetadata().getName());
							scd.setType(type.name());
							ServiceBinding binding = getServiceBinding(svc);
							if (binding == null) {
								scd.setBound(true);
							}
							sources.add(scd);
						}
					}
				}
			}
    	} catch (Exception e) {
    		throw handleError(e);
    	}
    	return sources;
	}

	private SourceType matchType(ServiceInstance svc, Map<String, String> parameters) {
		if (parameters.get("POSTGRESQL_DATABASE") != null) {
			return SourceType.postgresql;
		} else if (parameters.get("MYSQL_DATABASE") != null) {
			return SourceType.mysql;
		} else if (parameters.get("MONGODB_DATABASE") != null) {
			return SourceType.mongodb;
		}
		return null;
	}

	@Override
	public void bindToServiceCatalogSource(String dsName) throws KException {
		KLog.getLogger().info("Bind to Service" + dsName);
		String token = AuthHandlingFilter.threadOAuthCredentials.get().getToken();
		this.scClient.setAuthHeader(token);
		try {
			ServiceInstance svc = this.scClient.getServiceInstance(ApplicationProperties.getNamespace(), dsName);
			if (svc == null) {
				throw new KException("No Service Catalog Service found with name " + dsName);
			}
			
			ServiceBinding binding = getServiceBinding(svc);
			if (binding != null) {
				KLog.getLogger().debug("Found existing Binding = " + binding);
			}
			if (binding == null) {
				binding = this.scClient.createServiceBinding(svc);
				KLog.getLogger().debug("Created new Binding = " + binding);
			}
			
			//TODO: need to come up async based operation
			int i = 0;
			while(!binding.getStatus().isReady()) {
				Thread.sleep(5000);
				i++;
				binding = getServiceBinding(svc);
				if (i > 3 || binding == null) {
					throw new KException("Created Service Binding is not Ready");
				}
			}
			
			Map<String, String> parameters = getParameters(svc);
			SourceType type = matchType(svc, parameters);
			
			Collection<String> dsNames = admin().getDataSourceNames();
			if (!dsNames.contains(dsName)) {				
				Map<String, String> bindingSecrets = getBindingSecrets(binding);
				bindingSecrets.putAll(parameters);
				createDataSource(dsName, type, bindingSecrets);				
			}			
		} catch (Exception e) {
			throw handleError(e);
		}
	}    
    
	private Map<String, String> getBindingSecrets(ServiceBinding binding) throws IOException {
		Map<String, String> map = new TreeMap<>();
		if (binding != null) {
			String secretName = binding.getSpec().getSecretName();
			Secret secret = this.scClient.getSecret(ApplicationProperties.getNamespace(), secretName);
			map = secret.getData();
			Map<String, String> decodedMap = new TreeMap<>();
			if (map != null && !map.isEmpty()) {
				for (Map.Entry<String, String> entry:map.entrySet()) {
					String key = entry.getKey();
					String encodedValue = entry.getValue();
					decodedMap.put(key, new String(Base64.getDecoder().decode(encodedValue)));
				}
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
			KLog.getLogger().debug(svc.getMetadata().getName()+":No Parameters Secret found");
		}  
		return map;
    }
    
	private void createDataSource(String name, SourceType type, Map<String, String> map) throws AdminException, KException {
		KLog.getLogger().debug("Creating the Datasource = "+name + " with properties = "+map);
		
		String driverName = null;
		Set<String> templateNames = admin().getDataSourceTemplateNames();
		KLog.getLogger().debug("template names:"+templateNames);
		for (String template : templateNames) {
			// TODO: there is null entering from above call from getDataSourceTemplateNames need to investigate why
			if (template != null && template.startsWith(type.name())) {
				driverName = template;
				break;
			}
		}
		
		if (driverName == null) {
			throw new KException("No driver or resource adapter found for source type " + type);
		}
		
		if (type.equals(SourceType.postgresql)) {
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
			Properties props = new Properties();
			props.setProperty("connection-url", "jdbc:postgresql://" + map.get("DATABASE_SERVICE_NAME") + ":5432/"
					+ map.get("database_name"));
			props.setProperty("user-name", map.get("username"));
			props.setProperty("password", map.get("password"));
			admin().createDataSource(name, driverName, props);
		} else if (type.equals(SourceType.mysql)) {
			/*
			{
			  "DATABASE_SERVICE_NAME":"mariadb",
			  "MEMORY_LIMIT":"512Mi",
			  "MYSQL_DATABASE":"sampledb",
			  "NAMESPACE":"openshift",
			  "VOLUME_CAPACITY":"1Gi"
			  database-password
			  database-name
			  database-root-password
		      database-user
			} 
			 */
			Properties props = new Properties();
			props.setProperty("connection-url", "jdbc:mysql://"+map.get("DATABASE_SERVICE_NAME")+":3306/" 
					+ map.get("database-name"));
			props.setProperty("user-name", map.get("database-user"));
			props.setProperty("password", map.get("database-password"));
			admin().createDataSource(name, driverName, props);
		} else if (type.equals(SourceType.mongodb)) {
			/*
			{
				  "DATABASE_SERVICE_NAME":"mongodb",
				  "MEMORY_LIMIT":"512Mi",
				  "MONGODB_DATABASE":"sampledb",
				  "MONGODB_VERSION":"3.2",
				  "NAMESPACE":"openshift",
				  "VOLUME_CAPACITY":"1Gi"
				  database-password
				  database-name
				  database-admin-password
			      database-user				  
			}
			*/
			Properties props = new Properties();
			props.setProperty("RemoteServerList", map.get("DATABASE_SERVICE_NAME")+":27017");
			props.setProperty("Username", map.get("database-user"));
			props.setProperty("Password", map.get("database-password"));
			props.setProperty("Database", map.get("database-name"));
			props.setProperty("SecurityType", "SCRAM_SHA_1");
			props.setProperty("AuthDatabase", map.get("database-name"));
			props.setProperty("Ssl", "false");
			admin().createDataSource(name, driverName, props);
		}
	}
    
	static class ServiceCatalogDataSourceImpl implements ServiceCatalogDataSource {
		private String name;
		private String type;
		private boolean bound;
		
		@Override
		public String getName() {
			return name;
		}

		@Override
		public String getType() {
			return type;
		}

		@Override
		public boolean isBound() {
			return bound;
		}

		public void setName(String name) {
			this.name = name;
		}

		public void setType(String type) {
			this.type = type;
		}

		public void setBound(boolean bound) {
			this.bound = bound;
		}
	}
}
