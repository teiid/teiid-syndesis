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
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.rest.AbstractKEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class RestViewEditorState extends AbstractKEntity {

    /**
     * Label used for view editor state id
     */
    public static final String ID_LABEL = "id";

    /**
     * Label used for view editor start content
     */
    public static final String CONTENT_LABEL = "undoables";

    /*
     * The contents of the view editor state
     */
    private RestViewEditorStateCommand[] content = new RestViewEditorStateCommand[0];

    /**
     * Constructor for use when deserializing
     */
    public RestViewEditorState() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param viewEditorState the view editor state
     *
     * @throws KException if error occurs
     */
    public RestViewEditorState(URI baseUri, ViewEditorState viewEditorState, UnitOfWork transaction) throws KException {
        super(baseUri);

        setId(viewEditorState.getName(transaction));

        List<RestViewEditorStateCommand> cmdList = new ArrayList<>();
        for (ViewEditorStateCommand cmd : viewEditorState.getCommands(transaction)) {
            RestViewEditorStateCommand restCmd = new RestViewEditorStateCommand(baseUri, cmd, transaction);
            cmdList.add(restCmd);
        }

        this.content = cmdList.toArray(new RestViewEditorStateCommand[0]);
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

    /**
     * @return the content
     */
    public RestViewEditorStateCommand[] getContent() {
        return content;
    }

    public void setContent(RestViewEditorStateCommand[] content) {
        this.content = content;
    }
}