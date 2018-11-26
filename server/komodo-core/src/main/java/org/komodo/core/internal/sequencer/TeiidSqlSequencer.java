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
package org.komodo.core.internal.sequencer;

import java.io.IOException;
import java.io.InputStream;

import javax.jcr.Binary;
import javax.jcr.NamespaceRegistry;
import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;

import org.komodo.metadata.DefaultMetadataInstance;
import org.komodo.utils.KLog;
import org.modeshape.common.annotation.NotThreadSafe;
import org.modeshape.common.text.ParsingException;
import org.modeshape.common.util.CheckArg;
import org.modeshape.common.util.IoUtil;
import org.modeshape.jcr.api.nodetype.NodeTypeManager;
import org.modeshape.jcr.api.sequencer.Sequencer;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.LanguageObject;


/**
 * A sequencer for Teiid SQL files.
 */
@NotThreadSafe
public class TeiidSqlSequencer extends Sequencer {

    private static final KLog LOGGER = KLog.getLogger();

    @Override
    public void initialize(NamespaceRegistry registry, NodeTypeManager nodeTypeManager) throws RepositoryException, IOException {
        registerNodeTypes("/config/TeiidSql.cnd", nodeTypeManager, true); //$NON-NLS-1$
    }

    @Override
    public boolean execute(Property inputProperty, Node outputNode, Context context) throws Exception {
        Binary sqlContent = inputProperty.getBinary();
        CheckArg.isNotNull(sqlContent, "teiid sql content binary value"); //$NON-NLS-1$

        InputStream stream = sqlContent.getStream();
        try {
            String sql = IoUtil.read(stream);
            convertToJcr(sql, outputNode);
        } catch (ParsingException e) {
            LOGGER.error(Messages.getString(Messages.TeiidSqlSequencer.ErrorParsingContent), e, e.getLocalizedMessage());
            throw e;
        } catch (Throwable e) {
            LOGGER.error(Messages.getString(Messages.TeiidSqlSequencer.ErrorSequencingContent), e, e.getLocalizedMessage());
            throw new Exception(e);
        } finally {
            stream.close();
        }

        return true;
    }

    public void convertToJcr(String sql, Node parent) throws Exception {
        if (sql == null)
            return;
        LanguageObject command = QueryParser.getQueryParser().parseDesignerCommand(sql);
        NodeGenerator generator = new NodeGenerator(parent,
                                                    DefaultMetadataInstance.dataTypeService(),
                                                    DefaultMetadataInstance.metadataVersion());
        generator.visitObject(command);
        if (generator.errorOccurred())
            throw generator.getError();
    }
}
