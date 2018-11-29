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
package org.komodo.rest.swagger;

import java.lang.reflect.Type;
import java.util.Iterator;
import org.komodo.rest.RestProperty;
import org.komodo.spi.repository.KomodoType;
import io.swagger.converter.ModelConverter;
import io.swagger.converter.ModelConverterContext;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;

/**
 * Converter to display properties of {@link RestProperty} class in swagger
 */
public class RestPropertyConverter extends RestEntityConverter<RestProperty> {

    @Override
    protected Class<RestProperty> getEntityClass() {
        return RestProperty.class;
    }

    @Override
    protected KomodoType getKomodoType() {
        return KomodoType.UNKNOWN; // Not required
    }

    @Override
    protected void addProperties(ModelImpl model, ModelConverterContext context) throws Exception {
        model.property(RestProperty.NAME_LABEL, requiredProperty(String.class));
        model.property(RestProperty.VALUE_LABEL, property(String.class));
    }

    @SuppressWarnings( "nls" )
    @Override
    public Model resolve(Type type, ModelConverterContext context, Iterator<ModelConverter> chain) {

        if (!isApplicable(type, getEntityClass()))
            return defaultAction(type, context, chain);

        ModelImpl model;
        try {
            model = new ModelImpl();
            model.setName(getEntityClass().getSimpleName());
            addProperties(model, context);

        } catch (Exception ex) {
            LOGGER.error("Exception occurred whilst resolving the model type " + type.toString());
            return null;
        }

        return model;
    }
}
