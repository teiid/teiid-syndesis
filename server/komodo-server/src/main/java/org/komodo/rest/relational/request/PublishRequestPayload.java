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
package org.komodo.rest.relational.request;

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;


@JsonSerialize
@JsonInclude(value=Include.NON_NULL)
public class PublishRequestPayload implements KRestEntity {

    @JsonProperty
    private String name;

    @JsonProperty("cpu-units")
    private Integer cpuUnits = 500;

    @JsonProperty
    private Integer memory = 1024;

    @JsonProperty("disk-size")
    private Integer diskSize = 20;

    @JsonProperty("enable-odata")
    private Boolean enableOdata = true;

    @Override
    @JsonIgnore
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    @JsonIgnore
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getCpuUnits() {
        return cpuUnits;
    }

    public void setCpuUnits(Integer cpuUnits) {
        this.cpuUnits = cpuUnits;
    }

    public Integer getMemory() {
        return memory;
    }

    public void setMemory(Integer memory) {
        this.memory = memory;
    }

    public Integer getDiskSize() {
        return diskSize;
    }

    public void setDiskSize(Integer diskSize) {
        this.diskSize = diskSize;
    }

    public Boolean getEnableOdata() {
        return enableOdata;
    }

    public void setEnableOdata(Boolean enableOdata) {
        this.enableOdata = enableOdata;
    }
}
