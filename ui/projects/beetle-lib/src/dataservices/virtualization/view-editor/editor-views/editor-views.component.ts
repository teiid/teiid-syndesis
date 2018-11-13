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

import { Component, OnDestroy, OnInit, ViewEncapsulation } from '@angular/core';
import { LoggerService } from "../../../../core/logger.service";
import { ViewEditorService } from "../../../virtualization/view-editor/view-editor.service";
import { ViewEditorPart } from "../../../virtualization/view-editor/view-editor-part.enum";
import { ViewEditorEvent } from "../../../virtualization/view-editor/event/view-editor-event";
import { Subscription } from "rxjs/Subscription";
import { ViewEditorI18n } from "../../../virtualization/view-editor/view-editor-i18n";

@Component({
  encapsulation: ViewEncapsulation.None,
  selector: 'app-editor-views',
  templateUrl: './editor-views.component.html',
  styleUrls: ['./editor-views.component.css']
})
export class EditorViewsComponent implements OnInit, OnDestroy {

  // text used by html
  public readonly messagesTabName = ViewEditorI18n.messagesTabName;
  public readonly previewTabName = ViewEditorI18n.previewTabName;

  private readonly previewIndex = 0;
  private readonly messagesIndex = 1;

  private readonly editorService: ViewEditorService;
  private readonly logger: LoggerService;
  private subscription: Subscription;

  /**
   * The tabs component configuration.
   */
  public tabs = [
    {
      "active": true // preview
    },
    {
      "active": false // message log
    },
  ];

  constructor( editorService: ViewEditorService,
               logger: LoggerService ) {
    this.editorService = editorService;
    this.logger = logger;
  }

  /**
   * @param {ViewEditorEvent} event the event being processed
   */
  public handleEditorEvent( event: ViewEditorEvent ): void {
    this.logger.debug( "EditorViewsComponent received event: " + event.toString() );

    if ( event.typeIsShowEditorPart() ) {
      if ( event.args.length !== 0 ) {
        if ( event.args[ 0 ] === ViewEditorPart.PREVIEW ) {
          this.tabs[ this.messagesIndex ].active = false;
          this.tabs[ this.previewIndex ].active = true;
        } else if ( event.args[ 0 ] === ViewEditorPart.MESSAGE_LOG ) {
          this.tabs[ this.previewIndex ].active = false;
          this.tabs[ this.messagesIndex ].active = true;
        }
      }
    }
  }

  /**
   * Cleanup code when destroying the view editor header.
   */
  public ngOnDestroy(): void {
    this.subscription.unsubscribe();
  }

  /**
   * Initialization code run after construction.
   */
  public ngOnInit(): void {
    this.subscription = this.editorService.editorEvent.subscribe( ( event ) => this.handleEditorEvent( event ) );
  }

  /**
   * Callback for when a tab is clicked.
   *
   * @param tab the tab being select or deselected
   * @param selected `true` is selected
   */
  public tabSelected( tab, selected ): void {
    tab.active = selected;
  }

}
