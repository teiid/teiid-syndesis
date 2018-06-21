/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.relational.response.vieweditorstate;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.rest.AbstractKEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class RestViewEditorStateCommand extends AbstractKEntity {

    /**
     * Label used for view editor state command id
     */
    public static final String ID_LABEL = "id";

    /**
     * Label used for view editor state command arguments
     */
    public static final String ARGS_LABEL = "args";

    private Map<String, String> arguments = new LinkedHashMap<>();

    /**
     * Constructor for use when deserializing
     */
    public RestViewEditorStateCommand() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param viewEditorStateCommand the view editor state command
     *
     * @throws KException if error occurs
     */
    public RestViewEditorStateCommand(URI baseUri, ViewEditorStateCommand viewEditorStateCmd, UnitOfWork transaction) throws KException {
        super(baseUri);

        setId(viewEditorStateCmd.getName(transaction));

        Map<String, String> arguments = viewEditorStateCmd.getArguments(transaction);
        for (Map.Entry<String, String> arg : arguments.entrySet()) {
            addArgument(arg.getKey(), arg.getValue());
        }
    }

    /**
     * @return the id
     */
    public String getId() {
        Object id = tuples.get(ID_LABEL);
        return id != null ? id.toString() : null;
    }

    public void setId(String name) {
        tuples.put(ID_LABEL, name);
    }

    public void addArgument(String name, String value) {
        arguments.put(name, value);
    }

    public Map<String, String> getArguments() {
        return arguments;
    }
}
