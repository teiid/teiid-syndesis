/*************************************************************************************
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
package org.komodo.importer;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.komodo.spi.StringConstants;
import org.komodo.utils.ArgCheck;

/**
 * ImportMessages
 * Holds different types of import messages for DDL import
 */
public class ImportMessages implements StringConstants {

    // Parse Error Message Details
    private String parseErrorMessage;
    private String parserId;
    private int parseErrorLineNumber = -1;
    private int parseErrorColNumber = -1;
    private int parseErrorIndex = -1;

    // Progress Messages
    private List<String> progressMessages;
    // Unhandled Type Messages
    private Map<String, Integer> unhandledTypeCountMap;
    // Error Messages
    private List<String> errorMessages;

    /**
     * Constructor
     */
    public ImportMessages() {
    }

    /**
     * Clear the messages
     */
    public void clear() {
        this.parseErrorMessage = null;
        this.parserId = null;
        this.parseErrorLineNumber = -1;
        this.parseErrorColNumber = -1;
        this.parseErrorIndex = -1;

        this.errorMessages = null;
        this.progressMessages = null;
        this.unhandledTypeCountMap = null;
    }

    /**
     * Add an error message based on the supplied exception.
     * @param exception the exception
     */
    public void addErrorMessage(Throwable exception) {
        ArgCheck.isNotNull(exception, "error"); //$NON-NLS-1$

    	while (exception.getCause() != null) {
    		//
    		// Zero down to the root cause of the exception
    		//
    		exception = exception.getCause();
    	}

    	String message = exception.getLocalizedMessage();
    	if (exception.getClass().getName().equals("TeiidDdlParsingException")) {
    	    try {
    	        Method posMethod = exception.getClass().getMethod("getPosition");
    	        Object position = posMethod.invoke(exception);
    	        message = Messages.getString(Messages.IMPORTER.teiidParserException,
    	                                     exception.getMessage(),
    	                                     position.toString());
    	    } catch (Exception ex) {
    	        message = exception.getMessage();
    	    }
    	}

    	errorMessages.add(message);
    }


    /**
     * Add an error message
     * @param message the error message
     */
    public void addErrorMessage(String message) {
        ArgCheck.isNotNull(message, "error message"); //$NON-NLS-1$

        if (errorMessages == null) {
            errorMessages = new ArrayList<String>();
        }
        errorMessages.add(message);
    }

    /**
     * Get the error messages
     * @return messages
     */
    public List<String> getErrorMessages() {
        if (errorMessages == null) {
            errorMessages = new ArrayList<String>();
        }

        return errorMessages;
    }

    /**
     * Get the error messages
     * @return error messages
     */
    public String errorMessagesToString() {
        if (errorMessages == null)
            return EMPTY_STRING;

        StringBuffer errorMsgs = new StringBuffer();
        for (String errorMsg : errorMessages) {
            errorMsgs.append(errorMsg);
            errorMsgs.append(NEW_LINE);
        }

        return errorMsgs.toString();
    }

    /**
     * Add a progress messages
     * @param message the progress message
     */
    public void addProgressMessage(String message) {
        if (progressMessages == null) {
            progressMessages = new ArrayList<String>();
        }
        progressMessages.add(message);
    }

    /**
     * Get the progress messages
     * @return messages
     */
    public List<String> getProgressMessages() {
        if (progressMessages == null) {
            progressMessages = new ArrayList<String>();
        }

        return progressMessages;
    }

    /**
     * Get the unhandled type messages
     * @return messages
     */
    public List<String> getUnhandledTypeMessages() {
        List<String> unhandledTypeMessages = new ArrayList<String>();
        if (unhandledTypeCountMap == null) {
            return new ArrayList<String>();
        }

        Iterator<String> keyIter = unhandledTypeCountMap.keySet().iterator();
        while (keyIter.hasNext()) {
            String typeStr = keyIter.next();
            Integer typeCount = unhandledTypeCountMap.get(typeStr);
            String message = typeCount + " instances of a DDL statement of type [" + typeStr + "] were found, but cannot be processed"; //$NON-NLS-1$ //$NON-NLS-2$
            unhandledTypeMessages.add(message);
        }

        return unhandledTypeMessages;
    }

    /**
     * Get all messages - error, progress and unhandled type messages
     * @return messages
     */
    public List<String> getAllMessages() {
        // All messages consists of error, progress and unhandled type messages
        List<String> allMessages = new ArrayList<String>(getErrorMessages());

        allMessages.addAll(getProgressMessages());

        allMessages.addAll(getUnhandledTypeMessages());
        return allMessages;
    }

    /**
     * Increment count of unhandled instances of a particular type
     * @param typeStr the node mixin type string
     */
    public void incrementUnhandledNodeType(String typeStr) {
        if (unhandledTypeCountMap == null) {
            unhandledTypeCountMap = new HashMap<String, Integer>();
        }
        if (unhandledTypeCountMap.containsKey(typeStr)) {
            Integer count = unhandledTypeCountMap.get(typeStr);
            count += 1;
            unhandledTypeCountMap.put(typeStr, count);
        } else {
            unhandledTypeCountMap.put(typeStr, new Integer(1));
        }
    }

    /**
     * Set the parse error message
     * @param message the error message
     */
    public void setParseErrorMessage(String message) {
        this.parseErrorMessage = message;
        addErrorMessage(message);
    }

    /**
     * Get the parse error message
     * @return the parse error message
     */
    public String getParseErrorMessage() {
        return this.parseErrorMessage;
    }

    /**
     * Determine if this has a parse error or other type of error
     * @return 'true' if hasParseError
     */
    public boolean hasError() {
        return !getErrorMessages().isEmpty();
    }

    /**
     * Determine if this has a parse error
     * @return 'true' if hasParseError
     */
    public boolean hasParseError() {
        return this.parseErrorMessage != null;
    }

    /**
     * Get the parse error line number
     * @return the lineNumber
     */
    public int getParseErrorLineNumber() {
        return this.parseErrorLineNumber;
    }

    /**
     * set the parse error line number
     * @param lineNumber the lineNumber to set
     */
    public void setParseErrorLineNumber(int lineNumber) {
        this.parseErrorLineNumber = lineNumber;
    }

    /**
     * Get the parse error column number
     * @return the colNumber
     */
    public int getParseErrorColNumber() {
        return this.parseErrorColNumber;
    }

    /**
     * Set the parse error column number
     * @param colNumber the colNumber to set
     */
    public void setParseErrorColNumber(int colNumber) {
        this.parseErrorColNumber = colNumber;
    }

    /**
     * Get the parser id
     * @return the parserId
     */
    public String getParserId() {
        return parserId;
    }

    /**
     * Set the parser id
     * @param parserId the parserId to set
     */
    public void setParserId(String parserId) {
        this.parserId = parserId;
    }

    /**
     * Get the parse error index
     * @return the index
     */
    public int getParseErrorIndex() {
        return parseErrorIndex;
    }

    /**
     * Set the parse error index
     * @param index the index to set
     */
    public void setParseErrorIndex(int index) {
        this.parseErrorIndex = index;
    }

    @Override
    public String toString() {
    	List<String> allMessages = getAllMessages();
    	if(allMessages.size()==0) {
    		return "No Import Messages"; //$NON-NLS-1$
    	}
    	StringBuilder sb = new StringBuilder();
    	for(String message : allMessages) {
    		sb.append(message);
    		sb.append('\n');
    	}
    	return sb.toString();
    }

}
