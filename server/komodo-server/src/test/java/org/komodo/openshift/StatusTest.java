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

package org.komodo.openshift;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.KException;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.openshift.BuildStatus.Status;
import org.komodo.rest.KomodoJsonMarshaller;

public class StatusTest {
	
	@Test public void testJsonRoundtrip() throws KException {
		BuildStatus bs = new BuildStatus("vdb");
		bs.setBuildName("buildName");
		bs.setDeploymentName("deploymentName");
		bs.setNamespace("namespace");
		bs.setPublishPodName("pod");
		bs.setStatus(Status.DEPLOYING);
		//not used by serialization
		bs.setPublishConfiguration(new PublishConfiguration());
		RouteStatus route = new RouteStatus("x", ProtocolType.JDBC);
		route.setHost("host");
		route.setPath("path");
		route.setPort("port");
		route.setSecure(true);
		route.setTarget("target");
		bs.addRoute(route);
		bs.addRoute(new RouteStatus("y", ProtocolType.ODATA));
		
		String value = KomodoJsonMarshaller.marshall(bs);
		assertEquals("{\n" + 
				"  \"build_status\" : \"DEPLOYING\",\n" + 
				"  \"build_name\" : \"buildName\",\n" + 
				"  \"deployment_name\" : \"deploymentName\",\n" + 
				"  \"vdb_name\" : \"vdb\",\n" + 
				"  \"namespace\" : \"namespace\",\n" + 
				"  \"last_updated\" : 0,\n" + 
				"  \"routes\" : [ {\n" + 
				"    \"name\" : \"x\",\n" + 
				"    \"protocol\" : \"jdbc\",\n" + 
				"    \"host\" : \"host\",\n" + 
				"    \"path\" : \"path\",\n" + 
				"    \"target\" : \"target\",\n" + 
				"    \"port\" : \"port\",\n" + 
				"    \"secure\" : true\n" + 
				"  }, {\n" + 
				"    \"name\" : \"y\",\n" + 
				"    \"protocol\" : \"odata\",\n" + 
				"    \"secure\" : false\n" + 
				"  } ]\n" + 
				"}", value);
	}

}
