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

export class PropertyDefinition<T> {
  private keng__id: string;
  private value: T;
  private displayName: string;
  private defaultValue: string;
  private description: string;
  private category: string;
  private typeClassName: string;
  private required: boolean;
  private advanced: boolean;
  private masked: boolean;
  private modifiable: boolean;
  private constrainedToAllowedValues: boolean;
  private allowedValues: string[];

  /**
   * @param json the JSON representation of a Property
   * @returns the new PropertyDefinition (never null)
   */
  public static create( json: object = {} ): PropertyDefinition<any> {
    const property = new PropertyDefinition();
    property.setValues( json );
    return property;
  }

  constructor() {
    // nothing to do
  }

   /**
    * @returns the property value (can be null)
    */
  public getValue(): T {
     return this.value;
   }

  /**
   * @returns the property id
   */
  public getId(): string {
    return this.keng__id;
  }

  /**
   * @returns the property displayName (can be null)
   */
  public getDisplayName(): string {
    return this.displayName;
  }

  /**
   * @returns the property default value (can be null)
   */
  public get theDefaultValue(): string {
    return this.defaultValue;
  }

  /**
   * @returns the property description (can be null)
   */
  public getDescription(): string {
    return this.description;
  }

  /**
   * @returns the property category (can be null)
   */
  public getCategory(): string {
    return this.category;
  }

  /**
   * @returns the property typeClassName (can be null)
   */
  public getTypeClassName(): string {
    return this.typeClassName;
  }

  /**
   * @returns 'true' if required
   */
  public isRequired(): boolean {
    return this.required;
  }

  /**
   * @returns 'true' if advanced
   */
  public isAdvanced(): boolean {
    return this.advanced;
  }

  /**
   * @returns 'true' if masked
   */
  public isMasked(): boolean {
    return this.masked;
  }

  /**
   * @returns 'true' if modifiable
   */
  public isModifiable(): boolean {
    return this.modifiable;
  }

  /**
   * @returns 'true' if constrainedToAllowedValues
   */
  public isConstrainedToAllowedValues(): boolean {
    return this.constrainedToAllowedValues;
  }

  /**
   * @returns the array of allowed values
   */
  public getAllowedValues(): string[] {
    return this.allowedValues;
  }

  /**
   * @param value the property value
   */
  public setValue( value?: T ): void {
    this.value = value ? value : null;
  }

  /**
   * @param id the property id
   */
  public setId( id?: string ): void {
    this.keng__id = id ? id : null;
  }

  /**
   * @param displayName the property displayName
   */
  public setDisplayName( displayName?: string ): void {
    this.displayName = displayName ? displayName : null;
  }

  /**
   * @param defaultValue the property default value
   */
  public setDefaultValue(defaultValue: string): void {
    this.defaultValue = defaultValue;
  }

  /**
   * @param description the property description
   */
  public setDescription(description: string): void {
    this.description = description;
  }

  /**
   * @param category the property category
   */
  public setCategory(category: string): void {
    this.category = category;
  }

  /**
   * @param typeClassName the property typeClassName
   */
  public setTypeClassName(typeClassName: string): void {
    this.typeClassName = typeClassName;
  }

  /**
   * @param required 'true' if property is required
   */
  public setRequired( required?: boolean ): void {
    this.required = required ? required : null;
  }

  /**
   * @param advanced 'true' if property is advanced
   */
  public setAdvanced( advanced?: boolean ): void {
    this.advanced = advanced ? advanced : null;
  }

  /**
   * @param masked 'true' if property is masked
   */
  public setMasked( masked?: boolean ): void {
    this.masked = masked ? masked : null;
  }

  /**
   * @param modifiable 'true' if property is modifiable
   */
  public setModifiable( modifiable?: boolean ): void {
    this.modifiable = modifiable ? modifiable : null;
  }

  /**
   * @param constrained 'true' if property is constrained
   */
  public setConstraintedToAllowedValues( constrained?: boolean ): void {
    this.constrainedToAllowedValues = constrained ? constrained : null;
  }

  /**
   * @param allowedValues the array of allowed values
   */
  public setAllowedValues( allowedValues?: string[] ): void {
    this.allowedValues = allowedValues ? allowedValues : null;
  }

  /**
   * Set all object values using the supplied Connection json
   * @param values
   */
  public setValues(values: object = {}): void {
    Object.assign(this, values);
  }

}
