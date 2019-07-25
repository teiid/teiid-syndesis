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
package org.komodo.relational.internal;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.profile.internal.ProfileImpl;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.KeyInValueMap;
import org.komodo.utils.KeyInValueMap.KeyFromValueAdapter;
import org.teiid.query.resolver.util.AccessPattern;

/**
 *
 */
public class TypeResolverRegistry {

    private class KTypeAdapter implements KeyFromValueAdapter<KomodoType, TypeResolver<?>> {
        @Override
        public KomodoType getKey(TypeResolver<?> value) {
            return value.identifier();
        }
    }

    private static TypeResolverRegistry instance;

    /**
     * @return singleton instance
     */
    public static final TypeResolverRegistry getInstance() {
        if (instance == null)
            instance = new TypeResolverRegistry();

        return instance;
    }

    private KeyInValueMap<KomodoType, TypeResolver<?>> kTypeIndex =
                    new KeyInValueMap<KomodoType, TypeResolver<?>>(new KTypeAdapter());

    private Map<Class<? extends KomodoObject>, TypeResolver<?>> kClassIndex =
                    new HashMap<Class<? extends KomodoObject>, TypeResolver<?>>();

    private TypeResolverRegistry() {


        index(KomodoType.DATASERVICE, DataserviceImpl.RESOLVER);


        index(KomodoType.PROFILE, ProfileImpl.RESOLVER);

    }

    @SuppressWarnings( "unchecked" )
    private void index(KomodoType kType, TypeResolver<?> resolver) {
        kTypeIndex.add(resolver);

        // Indexes the impl class
        Class<? extends KomodoObject> owningClass = resolver.owningClass();
        kClassIndex.put(owningClass, resolver);

        // Indexes the interface class
        Class<?>[] interfaces = owningClass.getInterfaces();
        if (interfaces.length > 0) {
            for (Class<?> iface : interfaces) {
                if (KomodoObject.class.isAssignableFrom(iface))
                    kClassIndex.put((Class<? extends KomodoObject>) iface, resolver);
            }
        }
    }

    /**
     * @return all registered resolvers
     */
    public Collection<TypeResolver<?>> getResolvers() {
      return Collections.unmodifiableCollection(kClassIndex.values());
    }

    /**
     * @param kType the komodo type
     * @return the {@link TypeResolver} for the given komodo type
     */
    public TypeResolver<?> getResolver(KomodoType kType) {
        if (kType == null || KomodoType.UNKNOWN.equals(kType))
            return null;

        return kTypeIndex.get(kType);
    }

    /**
     * @param kClass the resolver owning class or its interface, eg. {@link AccessPatternImpl} or {@link AccessPattern}
     * @return the {@link TypeResolver} for the given komodo class
     */
    public TypeResolver<?> getResolver(Class<? extends KomodoObject> kClass) {
        TypeResolver<?> resolver = kClassIndex.get(kClass);
         return resolver;
    }
}
