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
package org.komodo.relational.model.internal;

import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.LexiconConstants.JcrLexicon;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.UnitOfWork;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RelationalObjectImplTest extends RelationalModelTest {

    class RelationalTestObject extends RelationalObjectImpl {

        RelationalTestObject( final UnitOfWork uow,
                              final Repository repository,
                              final String path ) throws KException {
            super( uow, repository, path );
        }

    }

    private RelationalObject robject;

    @Before
    public void init() throws Exception {
        final KomodoObject model = createModel();
        this.robject = new RelationalTestObject( getTransaction(), RepositoryImpl.getRepository(getTransaction()), model.getAbsolutePath() );
        commit();
    }

    @Test
    public void shouldFilterJcrNamespace() throws Exception {

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( getTransaction() ) ) {
            if ( descriptor.getName().startsWith( JcrLexicon.Namespace.PREFIX ) ) {
                fail();
            }
        }
    }

    @Test
    public void shouldFilterResidual() throws Exception {

        for ( final PropertyDescriptor descriptor : this.robject.getPropertyDescriptors( getTransaction() ) ) {
            if ( "*".equals( descriptor.getName() ) ) {
                fail();
            }
        }
    }

}
