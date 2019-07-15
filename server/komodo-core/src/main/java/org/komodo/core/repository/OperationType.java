package org.komodo.core.repository;

/**
 * The nature of the operation being conducted
 * and to be vetted by the security system
 */
public enum OperationType {
    /**
     * Can read a node's attributes and get its children
     */
    READ_OPERATION,

    /**
     * Can add/remove children from a node but
     * cannot modify the node itself
     */
    CHILD_OPERATION,

    /**
     * Can modify a node's attributes
     */
    MODIFY_OPERATION,

    /**
     * Can a node be removed
     */
    REMOVE_OPERATION
}