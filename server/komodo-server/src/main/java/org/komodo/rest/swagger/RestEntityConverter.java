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

import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.Iterator;
import org.komodo.rest.RestLink;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.KLog;
import io.swagger.converter.ModelConverter;
import io.swagger.converter.ModelConverterContext;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;

/**
 * @param <T> Class to be converted
 */
public abstract class RestEntityConverter<T> implements ModelConverter, JsonConstants {

    protected static final KLog LOGGER = KLog.getLogger();

    protected abstract Class<T> getEntityClass();

    protected abstract KomodoType getKomodoType();

    protected abstract void addProperties(ModelImpl model, ModelConverterContext context) throws Exception;

    @Override
    public Property resolveProperty(Type type, ModelConverterContext context, Annotation[] annotations, Iterator<ModelConverter> chain) {
        if (chain.hasNext())
            return chain.next().resolveProperty(type, context, annotations, chain);

        return null;
    }

    protected boolean isApplicable(Type type, Class<T> klazz) {
        if (type == null)
            return false;

        //
        // Workaround for SWARM-1666. Cannot use Json.mapper().constructType(type)
        // since this causes a LinkageError due to an underlying static being already loaded
        // via the swarm-swagger fraction. The latter uses a different version of com.fasterxml (2.8.4)
        // to the rest of swarm (2.7.4) hence the LinkageError.
        //
        //      JavaType _type = Json.mapper().constructType(type);
        //      if (_type == null)
        //          return false;
        //
        if (type instanceof Class<?>) {
            Class<?> cls = (Class<?>)type;
            if (klazz.isAssignableFrom(cls)) {
                return true;
            }
        }
        return false;
    }

    protected Model defaultAction(Type type, ModelConverterContext context, Iterator<ModelConverter> chain) {
        if (chain.hasNext())
            return chain.next().resolve(type, context, chain);

        return null;
    }

    protected ModelImpl generateRestEntityModel(String name, ModelConverterContext context) throws Exception {
        ModelImpl model = new ModelImpl();
        model.setName(name);

        model.property(ID, requiredProperty(String.class));
        model.property(DATA_PATH, requiredProperty(String.class));

        StringProperty kTypeProperty = new StringProperty();
        kTypeProperty.setRequired(true);
        kTypeProperty.readOnly();
        kTypeProperty._enum(getKomodoType().toString());
        model.property(KTYPE, kTypeProperty);

        model.property(HAS_CHILDREN, requiredProperty(Boolean.class));

        model.property(LINKS, context.resolveProperty(RestLink.class, null));

        context.defineModel(model.getName(), model);

        return model;
    }

    protected Property property(Class<?> typeClass) throws Exception {
        Property property = null;
        if (String.class.equals(typeClass))
            property = new StringProperty();
        else if (Integer.class.equals(typeClass))
            property = new IntegerProperty();
        else if (Boolean.class.equals(typeClass))
            property = new BooleanProperty();
        else
            throw new Exception("Unsupported property type " +  typeClass); //$NON-NLS-1$
        return property;
    }

    protected Property requiredProperty(Class<?> typeClass) throws Exception {
        Property property = property(typeClass);
        property.setRequired(true);
        return property;
    }

    @SuppressWarnings( "nls" )
    @Override
    public Model resolve(Type type, ModelConverterContext context, Iterator<ModelConverter> chain) {
        if (!isApplicable(type, getEntityClass()))
            return defaultAction(type, context, chain);

        ModelImpl model;
        try {
            model = generateRestEntityModel(getEntityClass().getSimpleName(), context);
            addProperties(model, context);

        } catch (Exception ex) {
            LOGGER.error("Exception occurred whilst resolving the model type " + type.toString());
            return null;
        }

        return model;
    }
}
