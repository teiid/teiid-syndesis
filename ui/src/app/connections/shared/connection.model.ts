/**
 * @license
 * Copyright 2017 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import { ConnectionStatus } from "../../connections/shared/connection-status";
import { VdbsConstants } from "../../dataservices/shared/vdbs-constants";
import { Identifiable } from "../../shared/identifiable";
import { SortDirection } from "../../shared/sort-direction.enum";

export class Connection implements Identifiable< string > {

  public static descriptionProp = "description";
  public static serviceCatalogSourceProp = "serviceCatalogSource";

  private keng__id: string;
  private keng__dataPath: string;
  private dv__jndiName: string;
  private dv__driverName: string;
  private dv__type: boolean;
  private keng__properties: object[] = [];
  private status: ConnectionStatus;

  /**
   * @param json the JSON representation of a Connection
   * @returns the new Connection (never null)
   */
  public static create( json: object = {} ): Connection {
    const conn = new Connection();
    conn.setValues( json );
    return conn;
  }

  /**
   * @param connections the connections being sorted
   * @param sortDirection the sort direction
   */
  public static sort( connections: Connection[],
                      sortDirection: SortDirection ): void {
    connections.sort( ( thisConnection: Connection, thatConnection: Connection ) => {
      const result = thisConnection.compareTo( thatConnection );

      if ( sortDirection === SortDirection.DESC ) {
        return result * -1;
      }

      return result;
    } );
  }

  constructor() {
    // nothing to do
  }

  /**
   * See {Identifiable}.
   */
  public compareTo( that: Connection ): number {
    let result = 0;

    if ( this.getId() ) {
      if ( that.getId() ) {
        // both have an ID
        result = this.getId().localeCompare( that.getId() );
      } else {
        // thatItem does not have an ID
        result = 1;
      }
    } else if ( that.getId() ) {
      // thisItem does not have an ID and thatItem does
      result = -1;
    }

    return result;
  }

  /**
   * @returns the connection name
   */
  public get name(): string {
    return this.keng__id;
  }

  /**
   * @returns the connection description
   */
  public getDescription(): string {
    let description: string = null;
    for (const propMap of this.keng__properties) {
      if (propMap["name"] === Connection.descriptionProp) {
        description = propMap["value"];
        break;
      }
    }
    return description;
  }

  /**
   * @returns the connection driver name (can be null)
   */
  public getDriverName(): string {
    return this.dv__driverName;
  }

  /**
   * @returns the connection identifier (can be null)
   */
  public getId(): string {
    return this.keng__id;
  }

  /**
   * @returns the connection data path (can be null)
   */
  public getDataPath(): string {
    return this.keng__dataPath;
  }

  /**
   * @returns the connection JNDI name (can be null)
   */
  public getJndiName(): string {
    return this.dv__jndiName;
  }

  /**
   * @returns the jdbc status (true == jdbc)
   */
  public isJdbc(): boolean {
    return this.dv__type;
  }

  /**
   * @returns the service catalog source name
   */
  public getServiceCatalogSourceName(): string {
    let serviceCatalogName: string = null;
    for (const propMap of this.keng__properties) {
      if (propMap["name"] === Connection.serviceCatalogSourceProp) {
        serviceCatalogName = propMap["value"];
        break;
      }
    }
    return serviceCatalogName;
  }

  /**
   * Accessor to determine if connection overall status is active
   * @returns `true` if the overall status is active
   */
  public get isActive(): boolean {
    // vdb and schema status must both be active
    return this.serverVdbActive && this.schemaActive;
  }

  /**
   * Accessor to determine if connection overall status is inactive
   * @returns `true` if the overall status is inactive
   */
  public get isInactive(): boolean {
    // If vdb is missing or vdb active and schema missing - overall status is inactive
    return (this.serverVdbMissing || (this.serverVdbActive && this.schemaMissing));
  }

  /**
   * Accessor to determine if connection overall status is loading
   * @returns `true` if the overall status is loading
   */
  public get isLoading(): boolean {
    // If either the vdb or schema are loading - overall status is loading
    return (this.serverVdbLoading || (this.serverVdbActive && this.schemaLoading));
  }

  /**
   * Accessor to determine if connection overall status is failed
   * @returns `true` if the overall status is failed
   */
  public get isFailed(): boolean {
    return (this.serverVdbFailed || (this.serverVdbActive && this.schemaFailed));
  }

  /**
   * Accessor to determine if connection schema is active
   * @returns `true` if the schema is active
   */
  public get schemaActive(): boolean {
    return this.status.isSchemaAvailable();
  }

  /**
   * Accessor to determine if connection schema is missing
   * @returns `true` if the connection schema is missing
   */
  public get schemaMissing(): boolean {
    return this.status.isSchemaMissing();
  }

  /**
   * Accessor to determine if connection schema is loading
   * @returns `true` if the connection schema is loading
   */
  public get schemaLoading(): boolean {
    return this.status.isSchemaLoading();
  }

  /**
   * Accessor to determine if connection schema is failed
   * @returns `true` if the connection schema is in a failed state
   */
  public get schemaFailed(): boolean {
    return this.status.isSchemaFailed();
  }

  /**
   * @returns `true` if the connection server VDB is in an active state
   */
  public get serverVdbActive(): boolean {
    return this.status.isServerVdbActive();
  }

  /**
   * @returns `true` if the connection server VDB is in a failed state
   */
  public get serverVdbFailed(): boolean {
    return this.status.isServerVdbFailed();
  }

  /**
   * @returns `true` if the connection server VDB is loading
   */
  public get serverVdbLoading(): boolean {
    return this.status.isServerVdbLoading();
  }

  /**
   * @returns `true` if the server VDB is missing
   */
  public get serverVdbMissing(): boolean {
    return this.status.isServerVdbMissing();
  }

  /**
   * @returns the connection schema vdb name
   */
  public get schemaVdbName(): string {
    if (this.status && !(this.status == null)) {
      return this.status.getSchemaVdbName();
    }
    return this.deriveSchemaVdbName();
  }

  /**
   * @returns the connection schema vdb model name
   */
  public get schemaVdbModelName(): string {
    if (this.status && !(this.status == null)) {
      return this.status.getSchemaModelName();
    }
    return this.deriveSchemaVdbModelName();
  }

  /**
   * @returns the connection schema vdb model source name
   */
  public get schemaVdbModelSourceName(): string {
    return this.deriveSchemaVdbModelSourceName();
  }

  /**
   * @param driverName the connection driver name (optional)
   */
  public setDriverName( driverName?: string ): void {
    this.dv__driverName = driverName ? driverName : null;
  }

  /**
   * @param id the connection identifier (optional)
   */
  public setId( id?: string ): void {
    this.keng__id = id ? id : null;
  }

  /**
   * @param jndiName the connection JNDI name (optional)
   */
  public setJndiName( jndiName?: string ): void {
    this.dv__jndiName = jndiName ? jndiName : null;
  }

  /**
   * @param jdbc the jdbc state
   */
  public setJdbc( jdbc: boolean ): void {
    this.dv__type = jdbc;
  }

  /**
   * @param erviceCatalog the service catalog source name
   */
  public setServiceCatalogSourceName( serviceCatalog: string ): void {
    interface IProp {
      name?: string;
      value?: string;
    }
    const prop: IProp = {};
    prop.name = Connection.serviceCatalogSourceProp;
    prop.value = serviceCatalog;

    this.keng__properties.push(prop);
  }

  /**
   * @param status the connection status
   */
  public setStatus( status: ConnectionStatus ): void {
    this.status = status;
  }

  /**
   * Set all object values using the supplied Connection json
   * @param values
   */
  public setValues(values: object = {}): void {
    Object.assign(this, values);
  }

  /**
   * Derive the schema VDB name for this connection
   * @returns the default schema VDB name
   */
  private deriveSchemaVdbName( ): string {
    const name = this.getId() + VdbsConstants.SCHEMA_VDB_SUFFIX;
    return name.toLowerCase();
  }

  /**
   * Derive the schema VDB model name for this connection
   * @returns the default schema VDB model name
   */
  private deriveSchemaVdbModelName( ): string {
    const name = this.getId() + VdbsConstants.SCHEMA_MODEL_SUFFIX;
    return name.toLowerCase();
  }

  /**
   * Derive the schema VDB model source name for this connection
   * @returns name
   */
  private deriveSchemaVdbModelSourceName( ): string {
    return this.getServiceCatalogSourceName() ?
      this.getServiceCatalogSourceName() : this.getId().toLowerCase();
  }

}
