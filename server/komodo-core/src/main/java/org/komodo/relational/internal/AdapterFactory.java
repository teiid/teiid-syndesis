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

import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * Factory dedicated to adapting a {@link KomodoObject} into its
 * relational object instance.
 */
public class AdapterFactory {

    /**
     */
    public AdapterFactory() {
    }

    /**
     * Attempts to adapt the given object to a relational model typed class.
     * If the object is not an instance of {@link KomodoObject} then null is
     * returned.
     *
     * The type id of the {@link KomodoObject} is extracted and the correct
     * relational model object created. If the latter is not assignable from the
     * given adapted class then it is concluded the adaption should fail and
     * null is returned, otherwise the new object is returned.
     * @param <T> the result's type
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param object object to adapt
     * @param adaptedClass the expected class that the object should be adapted to
     * @return the adapted instance or null
     */
    @SuppressWarnings( "unchecked" )
    public <T> T adapt(UnitOfWork transaction, Object object, Class<T> adaptedClass) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (! (object instanceof KomodoObject))
            return null;

        if (adaptedClass.isInstance(object))
            return (T) object;

        RelationalObject result = null;

        try {
            KomodoObject kObject = (KomodoObject) object;
            KomodoType type = kObject.getTypeIdentifier();
            TypeResolverRegistry registry = TypeResolverRegistry.getInstance();
            TypeResolver< ? > resolver = registry.getResolver(type);

            if (resolver != null && resolver.resolvable(kObject))
                result = resolver.resolve(kObject);

            if (result == null) {
                // Failed with the type identifier so try to be safe than sorry
                // and iterate through all resolvers to check this object is really
                // not resolvable.
                for (final TypeResolver< ? > aResolver : registry.getResolvers()) {
                    if (aResolver.resolvable(kObject)) {
                        result = aResolver.resolve(kObject);
                        break;
                    }
                }
            }

            if (result == null)
                return null; // Type cannot be resolved so cannot be adapted

        } catch (final Exception e) {
            // No need to log error
        }

        if (result == null)
            return null;

        if (! adaptedClass.isAssignableFrom(result.getClass()))
            return null;

        return (T) result;
    }

}
