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
package org.komodo.core.repository.validation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Utilities for Validation Rules
 */
public class ValidationUtils implements StringConstants {

    private ValidationUtils() {}

    /**
     * Tests KomodoObect to determine if its type and properties pass the type and property restrictions.
     * @param transaction the transaction
     * @param kObj the KomodoObject
     * @param ruleNodeType the node type to verify
     * @param propRestrictions the property name-values that must be present on the supplied KomodoObject
     * @return <code>true</code> if the supplied KomodoObject type matches
     * @throws KException if an error is encountered.
     */
    public static boolean objectPassesTypeAndPropertyRestrictions ( final UnitOfWork transaction, 
                                                                    final KomodoObject kObj, 
                                                                    final String ruleNodeType, 
                                                                    final Map<String,String> propRestrictions ) throws KException {

        if( objectTypeMatches(transaction, kObj, ruleNodeType) && objectPropsPassRestrictions(transaction, kObj, propRestrictions) ) {
            return true;
        }
        return false;
    }
    
    /**
     * Tests KomodoObect to determine if its type matches the supplied type
     * @param transaction the transaction
     * @param kObj the KomodoObject
     * @param nodeType the node type to verify
     * @return <code>true</code> if the supplied KomodoObject type matches
     * @throws KException if an error is encountered.
     */
    public static boolean objectTypeMatches ( final UnitOfWork transaction, final KomodoObject kObj, final String nodeType ) throws KException {
        String primaryType = kObj.getPrimaryType(transaction).getName();
        if(primaryType.equals(nodeType)) {
            return true;
        }
        
        Descriptor[] mixinTypes = kObj.getDescriptors(transaction);
        for(Descriptor mixinType : mixinTypes) {
            if(mixinType.getName().equals(nodeType)) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Tests KomodoObect to determine if its properties pass the supplied restrictions.
     * @param transaction the transaction
     * @param kObj the KomodoObject
     * @param propRestrictions the property name-values that must be present on the supplied KomodoObject
     * @return <code>true</code> if the supplied KomodoObject type matches
     * @throws KException if an error is encountered.
     */
    public static boolean objectPropsPassRestrictions ( final UnitOfWork transaction, final KomodoObject kObj, final Map<String,String> propRestrictions ) throws KException {
        if(propRestrictions.isEmpty()) return true;
        
        String propKey = propRestrictions.keySet().iterator().next();
        String rqdValue = propRestrictions.get(propKey);
        
        // Check that object has the property
        if(!kObj.hasRawProperty(transaction, propKey)) {
            return false;
        }
        
        // Check that object has correct property value
        String objPropValue = kObj.getRawProperty(transaction, propKey).getStringValue(transaction);
        if(!rqdValue.equals(objPropValue)) {
            return false;
        }
        return true;
    }
    
    /**
     * Get the children of the supplied KomodoObject which match the specified type and property restrictions.
     * @param transaction the transaction
     * @param kObj the KomodoObject
     * @param nodeType the node type to verify
     * @param propRestrictions the property name-values that must be present on the children
     * @return the matching children
     * @throws KException if an error is encountered.
     */
    public static KomodoObject[] getChildrenMatchingTypeAndPropRestrictions(final UnitOfWork transaction, final KomodoObject kObj, final String nodeType, final Map<String,String> propRestrictions) throws KException {
        KomodoObject[] objectsOfType = kObj.getChildrenOfType( transaction, nodeType );
        if(propRestrictions.isEmpty()) return objectsOfType;
        
        List<KomodoObject> validList = new ArrayList<KomodoObject>();
        for(KomodoObject childObject : objectsOfType) {
            if( objectPropsPassRestrictions(transaction, childObject, propRestrictions) ) {
                validList.add(childObject);
            }
        }
        
        return validList.toArray(new KomodoObject[ validList.size() ]);
    }

}
