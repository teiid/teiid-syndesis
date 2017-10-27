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
package org.komodo.spi.utils;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A {@link Map} facade which stores values that also reference their
 * own keys.
 *
 * @param <K> key
 * @param <V> value
 */
@SuppressWarnings( "unchecked" )
public class KeyInValueMap<K, V> extends AbstractMap<K, V> {
	
	private final Map<K, V> instance;

    /**
     * Adapter interface that clients of the class should implement
     * and pass to its constructor so that the key (K) can be derived
     * from the value (V).
     *
     * @param <K> key
     * @param <V> value
     */
    public interface KeyFromValueAdapter<K, V> {

        /**
         * Get the key from the value
         *
         * @param value
         *
         * @return key (K) from the value (V)
         */
        K getKey(V value);

    }

    private KeyFromValueAdapter<K, V> adapter;

    /**
     * Create a new instance
     *
     * @param adapter that can convert from the value into the key
     */
    public KeyInValueMap(KeyFromValueAdapter<K, V> adapter) {
        this(adapter, new HashMap<>());
    }
    
    /**
     * Create a new instance, given the existing map
     *
     * @param adapter that can convert from the value into the key
     * @param map
     */
    public KeyInValueMap(KeyFromValueAdapter<K, V> adapter, Map<K, V> map) {
        this.adapter = adapter;
        this.instance = map;
    }

    @Override
    public V put(K key, V value) {
        throw new UnsupportedOperationException("Use add rather than put since the key is part of the value"); //$NON-NLS-1$
    }

    /**
     * Add a value to this map where its key will be derived
     * by the {@link KeyFromValueAdapter}
     *
     * @param value the Value
     *
     * @return true if the value was added.
     */
    public boolean add(V value) {
    	return instance.putIfAbsent(adapter.getKey(value), value) == null;
    }

    /**
     * Remove a value from this map where the key will
     * be determined by the {@link KeyFromValueAdapter}
     *
     * @param value the Value
     *
     * @return removed value or null.
     */
    @Override
    public V remove(Object value) {
    	try {
    		return instance.remove(adapter.getKey((V)value));
    	} catch (ClassCastException e) {
    		return instance.remove(value);
    	}
    }

	@Override
	public Set<Entry<K, V>> entrySet() {
		return instance.entrySet();
	}

}
