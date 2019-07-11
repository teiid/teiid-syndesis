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
package org.komodo.relational.dataservice.internal;

import java.util.Properties;

import org.komodo.metadata.internal.DocumentType;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.dataservice.ServiceVdbEntry;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * An implementation of a Service VDB entry in a data service.
 */
public class ServiceVdbEntryImpl extends RelationalObjectImpl implements ServiceVdbEntry {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { ServiceVdbEntry.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    public ServiceVdbEntryImpl( final UnitOfWork uow,
                                final Repository repository,
                                final String path ) throws KException {
    	super( uow, repository, path );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.internal.VdbEntryImpl#getArchiveFolder()
     */
    @Override
    public String getArchiveFolder() {
        return StringConstants.EMPTY_STRING;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork uow,
                          final Properties properties ) throws KException {
        final Vdb vdb = getReference( uow );

        if ( vdb == null ) {
            throw new KException( Messages.getString( Relational.EXPORT_FAILED_NO_CONTENT, getAbsolutePath() ) );
        }

        return vdb.export( uow, properties );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.DataServiceEntry#getReference(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public VdbImpl getReference( final UnitOfWork uow ) throws KException {
        if ( hasProperty( uow, DataVirtLexicon.VdbEntry.VDB_REF ) ) {
            final String refId = getProperty( uow, DataVirtLexicon.VdbEntry.VDB_REF ).getStringValue( uow );
            final KomodoObject kobj = getRepository().getUsingId( uow, refId );

            if ( kobj == null ) {
                throw new KException( Messages.getString( Messages.Relational.REFERENCED_RESOURCE_NOT_FOUND,
                		DataVirtLexicon.VdbEntry.VDB_REF,
                                                          refId ) );
            }

            return new VdbImpl( uow, getRepository(), kobj.getAbsolutePath() );
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#getVdbName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbName( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getVdbName", //$NON-NLS-1$
                                  DataVirtLexicon.VdbEntry.VDB_NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#getVdbVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbVersion( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getVdbVersion", //$NON-NLS-1$
                                  DataVirtLexicon.VdbEntry.VDB_VERSION );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#setVdbName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setVdbName( final UnitOfWork transaction,
                            final String vdbName ) throws KException {
        setObjectProperty( transaction, "setVdbName", DataVirtLexicon.VdbEntry.VDB_NAME, vdbName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.VdbEntry#setVdbVersion(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setVdbVersion( final UnitOfWork transaction,
                               final String vdbVersion ) throws KException {
        setObjectProperty( transaction, "setVdbVersion", DataVirtLexicon.VdbEntry.VDB_VERSION, vdbVersion ); //$NON-NLS-1$
    }
    
    @Override
    public String getEntryPath( final UnitOfWork transaction ) throws KException {
        if ( hasProperty( transaction, DataVirtLexicon.ResourceEntry.PATH ) ) {
            return getProperty( transaction, DataVirtLexicon.ResourceEntry.PATH ).getStringValue( transaction );
        }

        final VdbImpl file = getReference( transaction );
        String folder = getArchiveFolder();

        if ( StringUtils.isBlank( folder ) ) {
            if ( folder == null ) {
                folder = StringConstants.EMPTY_STRING;
            }
        } else if ( !folder.endsWith( StringConstants.FORWARD_SLASH ) ) {
            folder += StringConstants.FORWARD_SLASH;
        }

        if ( file != null ) {
            return ( folder + DocumentType.VDB_XML.fileName( file.getName( transaction ) ) );
        }

        return ( folder + getName( transaction ) );
    }
    
    @Override
    public PublishPolicy getPublishPolicy( final UnitOfWork transaction ) throws KException {
        if ( hasProperty( transaction, DataVirtLexicon.DataServiceEntry.PUBLISH_POLICY ) ) {
            final String value = getProperty( transaction,
                                              DataVirtLexicon.DataServiceEntry.PUBLISH_POLICY ).getStringValue( transaction );
            return PublishPolicy.valueOf( value );
        }

        return PublishPolicy.DEFAULT;
    }
    
    @Override
    public void setEntryPath( final UnitOfWork transaction,
            final String newEntryPath ) throws KException {
    	setProperty( transaction, DataVirtLexicon.DataServiceEntry.PATH, newEntryPath );
	}
    
    @Override
    public void setPublishPolicy( final UnitOfWork transaction,
            final PublishPolicy newPublishPolicy ) throws KException {
		String value = ( ( newPublishPolicy == null ) ? null : newPublishPolicy.name() );
		setProperty( transaction, DataVirtLexicon.DataServiceEntry.PUBLISH_POLICY, value );
	}
    
	@Override
	public void setReference(final UnitOfWork transaction, final Vdb reference) throws KException {
		String refId = null;

		if (reference != null) {
			Property uuidProperty = getObjectFactory().getId(transaction, reference);
			if (uuidProperty == null) {
				String msg = Messages.getString(Messages.Relational.NO_UUID_PROPERTY, reference.getName(transaction));
				throw new KException(msg);
			}

			refId = uuidProperty.getStringValue(transaction);
		}

		setProperty(transaction, DataVirtLexicon.DataServiceEntry.SOURCE_RESOURCE, refId);
	}

}
