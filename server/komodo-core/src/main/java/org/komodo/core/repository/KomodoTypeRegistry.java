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
package org.komodo.core.repository;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.komodo.core.KomodoLexicon;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;
import org.komodo.spi.lexicon.ddl.StandardDdlLexicon;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.utils.KeyInValueMap;
import org.komodo.spi.utils.KeyInValueMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;

/**
 *
 */
public class KomodoTypeRegistry implements StringConstants {

    /**
     * Identifier representation of a komodo relational type
     */
    public static class TypeIdentifier {

        private final KomodoType kType;

        private final String lexiconType;

        /**
         * Construct new instance
         *
         * @param kType the komodo type
         * @param lexiconType the node id
         */
        public TypeIdentifier(KomodoType kType, String lexiconType) {
            ArgCheck.isNotNull(kType);
            ArgCheck.isNotNull(lexiconType);

            this.kType = kType;
            this.lexiconType = lexiconType;
        }

        /**
         * @return the komodo type
         */
        public KomodoType getKomodoType() {
            return kType;
        }

        /**
         * @return the komodo type id
         */
        public String getKomodoTypeId() {
            return kType.getType();
        }

        /**
         * @return the lexiconType
         */
        public String getLexiconType() {
            return this.lexiconType;
        }

        @Override
        public String toString() {
            return "TypeIdentifier [kType=" + this.kType + ", lexiconType=" + this.lexiconType + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
    }

    private class KTypeAdapter implements KeyFromValueAdapter<KomodoType, TypeIdentifier> {
        @Override
        public KomodoType getKey(TypeIdentifier value) {
            return value.getKomodoType();
        }
    }

    private static KomodoTypeRegistry instance;

    /**
     * @return singleton instance
     */
    public static KomodoTypeRegistry getInstance() {
        if (instance == null)
            instance = new KomodoTypeRegistry();

        return instance;
    }

    private KeyInValueMap<KomodoType, TypeIdentifier> kTypeIndex =
                    new KeyInValueMap<>(new KTypeAdapter());

    private KomodoTypeRegistry() {

        index(KomodoType.ACCESS_PATTERN, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);

        index(KomodoType.COLUMN, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        index(KomodoType.DATA_TYPE_RESULT_SET, TeiidDdlLexicon.CreateProcedure.RESULT_DATA_TYPE);

        index(KomodoType.DATASERVICE, DataVirtLexicon.DataService.NODE_TYPE);

        index(KomodoType.CONNECTION, DataVirtLexicon.Connection.NODE_TYPE);

        index(KomodoType.UDF_FILE, DataVirtLexicon.ResourceFile.UDF_FILE_NODE_TYPE);
        index(KomodoType.UDF_ENTRY, DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE);

        index(KomodoType.DDL_FILE, DataVirtLexicon.ResourceFile.DDL_FILE_NODE_TYPE);
        index(KomodoType.DDL_FILE, DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE);

        index(KomodoType.RESOURCE, DataVirtLexicon.ResourceFile.NODE_TYPE);
        index(KomodoType.RESOURCE_ENTRY, DataVirtLexicon.ResourceEntry.NODE_TYPE);

        index(KomodoType.FOLDER, KomodoLexicon.Folder.NODE_TYPE);

        index(KomodoType.FOREIGN_KEY, TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT);

        index(KomodoType.GIT_REPOSITORY, KomodoLexicon.GitRepository.NODE_TYPE);

        index(KomodoType.INDEX, TeiidDdlLexicon.Constraint.INDEX_CONSTRAINT);

        index(KomodoType.MODEL, VdbLexicon.Vdb.DECLARATIVE_MODEL);

        index(KomodoType.PARAMETER, TeiidDdlLexicon.CreateProcedure.PARAMETER);

        index(KomodoType.PRIMARY_KEY, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);

        index(KomodoType.PROFILE, KomodoLexicon.Profile.NODE_TYPE);

        index(KomodoType.PUSHDOWN_FUNCTION, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT);

        index(KomodoType.RESULT_SET_COLUMN, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);

        index(KomodoType.SCHEMA, KomodoLexicon.Schema.NODE_TYPE);

        index(KomodoType.STATE_COMMAND, KomodoLexicon.StateCommand.NODE_TYPE);

        index(KomodoType.STATE_COMMAND_AGGREGATE, KomodoLexicon.StateCommandAggregate.NODE_TYPE);

        index(KomodoType.STATEMENT_OPTION, StandardDdlLexicon.TYPE_STATEMENT_OPTION);

        index(KomodoType.STORED_PROCEDURE, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        index(KomodoType.TABLE, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);

        index(KomodoType.TABULAR_RESULT_SET, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMNS);

        index(KomodoType.TEMPLATE, DataVirtLexicon.Template.NODE_TYPE);

        index(KomodoType.TEMPLATE_ENTRY, DataVirtLexicon.TemplateEntry.NODE_TYPE);

        index(KomodoType.UNIQUE_CONSTRAINT, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);

        index(KomodoType.USER_DEFINED_FUNCTION, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT);

        index(KomodoType.VIEW_EDITOR_STATE, KomodoLexicon.ViewEditorState.NODE_TYPE);

        index(KomodoType.VIRTUAL_PROCEDURE, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        index(KomodoType.VDB, VdbLexicon.Vdb.VIRTUAL_DATABASE);

        index(KomodoType.VDB_CONDITION, VdbLexicon.DataRole.Permission.Condition.CONDITION);

        index(KomodoType.VDB_DATA_ROLE, VdbLexicon.DataRole.DATA_ROLE);

        index(KomodoType.VDB_ENTRY, VdbLexicon.Entry.ENTRY);

        index(KomodoType.VDB_IMPORT, VdbLexicon.ImportVdb.IMPORT_VDB);

        index(KomodoType.VDB_MASK, VdbLexicon.DataRole.Permission.Mask.MASK);

        index(KomodoType.VDB_MODEL_SOURCE, VdbLexicon.Source.SOURCE);

        index(KomodoType.VDB_PERMISSION, VdbLexicon.DataRole.Permission.PERMISSION);

        index(KomodoType.VDB_TRANSLATOR, VdbLexicon.Translator.TRANSLATOR);

        index(KomodoType.VIEW, TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);

        index(KomodoType.DDL_SCHEMA, TeiidDdlLexicon.Namespace.PREFIX);

        index(KomodoType.TSQL_SCHEMA, TeiidSqlLexicon.Namespace.PREFIX);

        index(KomodoType.VDB_SCHEMA, VdbLexicon.Namespace.PREFIX);
    }

    private void index(KomodoType kType, String lexiconType) {
        TypeIdentifier identifier = new TypeIdentifier(kType, lexiconType);
        kTypeIndex.add(identifier);
    }

    /**
     * @param kType the komodo type
     * @return the {@link TypeIdentifier} for the given komodo type
     */
    public TypeIdentifier getIdentifier(KomodoType kType) {
        return kTypeIndex.get(kType);
    }

    /**
     * @param lexiconType the lexicon identified type
     * @return all the type identifiers with the given lexicon type
     */
    public Set<TypeIdentifier> getIdentifiers(String lexiconType) {
        if (lexiconType == null)
            return Collections.emptySet();

        Set<TypeIdentifier> identifiers = new HashSet<>();
        for (TypeIdentifier identifier : kTypeIndex.values()) {
            if (identifier.getLexiconType().equals(lexiconType))
                identifiers.add(identifier);
        }

        if (! identifiers.isEmpty())
            return identifiers;

        //
        // We want to return TSQL for Teiid SQL nodes
        // but do not want to index all of them.
        //
        if (lexiconType.startsWith(TeiidSqlLexicon.Namespace.PREFIX)) {
            identifiers.add(kTypeIndex.get(KomodoType.TSQL_SCHEMA));
            return identifiers;
        }

        //
        // We want to return DDL for ddl nodes that do not have explicit types
        //
        if (lexiconType.startsWith(TeiidDdlLexicon.Namespace.PREFIX)) {
            identifiers.add(kTypeIndex.get(KomodoType.DDL_SCHEMA));
            return identifiers;
        }

        //
        // We want to return VDB for vdb nodes that do not have explicit types
        //
        if (lexiconType.startsWith(VdbLexicon.Namespace.PREFIX)) {
            identifiers.add(kTypeIndex.get(KomodoType.VDB_SCHEMA));
            return identifiers;
        }

        return identifiers;
    }
}
