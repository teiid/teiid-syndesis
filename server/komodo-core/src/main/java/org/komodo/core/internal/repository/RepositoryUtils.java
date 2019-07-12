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
package org.komodo.core.internal.repository;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.RepositoryException;
import javax.jcr.nodetype.NodeType;

import org.komodo.spi.StringConstants;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.api.Session;

/**
 *
 */
class RepositoryUtils implements StringConstants {

    private RepositoryUtils() {}

    /**
     * @param engine the engine to check
     *
     * @return true if engine is running
     */
    static boolean isEngineRunning(ModeShapeEngine engine) {
        if (engine == null)
            return false;

        return ModeShapeEngine.State.RUNNING.equals(engine.getState());
    }

    /**
     * @param repository the repository to check
     *
     * @return true if repository is running
     */
    static boolean isRepositoryRunning(JcrRepository repository) {
        if (repository == null)
            return false;

        return ModeShapeEngine.State.RUNNING.equals(repository.getState());
    }

    /**
     * Generates a modeshape-api version of {@link Session} to ensure that {@link KSequencers}
     * is able to fire the sequencers manually. All other uses of the created session will simply
     * be {@link javax.jcr.Session}
     *
     * @param identifier identifier of the workspace
     *
     * @return a new session for the given repository
     * @throws Exception if an error occurs
     */
    static Session createSession(WorkspaceIdentifier identifier) throws Exception {
        return JcrUowDelegateImpl.generateSession(identifier);
    }

    /**
     * @param node the node
     *
     * @return the primary and mixin node types of the node
     * @throws RepositoryException if error occurs
     */
    static List<NodeType> getAllNodeTypes(Node node) throws RepositoryException {
        List<NodeType> nodeTypes = new ArrayList<NodeType>();
        nodeTypes.add(node.getPrimaryNodeType());
        nodeTypes.addAll(Arrays.asList(node.getMixinNodeTypes()));
        return nodeTypes;
    }

    /**
     * @param node the node
     * @param namespace the type namespace
     * @return true if the node has any types with the given namespace
     * @throws RepositoryException if error occurs
     */
    public static boolean hasTypeNamespace(Node node, String namespace) throws RepositoryException {
        for (String name : getAllNodeTypeNames(node)) {
            if (name.startsWith(namespace + COLON))
                return true;
        }

        return false;
    }

    /**
     * @param node the node
     *
     * @return the primary and mixin node types of the node
     * @throws RepositoryException if error occurs
     */
    public static List<String> getAllNodeTypeNames(Node node) throws RepositoryException {
        List<String> nodeTypes = new ArrayList<String>();

        nodeTypes.add(node.getPrimaryNodeType().getName());
        for (NodeType nodeType : node.getMixinNodeTypes()) {
            nodeTypes.add(nodeType.getName());
        }

        return nodeTypes;
    }

    /**
     * @param node the node
     *
     * @return number of children belonging to this node
     * @throws RepositoryException if error occurs
     */
    public static int childrenCount(Node node) throws RepositoryException {
        NodeIterator iterator = node.getNodes();
        int count = 0;

        while (iterator.hasNext()) {
            count++;
            iterator.next();
        }

        return count;
    }

}
