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

import { Component, Input, OnInit, TemplateRef, ViewEncapsulation } from "@angular/core";
import { Router } from "@angular/router";
import { ConnectionsConstants } from "../../connections/shared/connections-constants";
import { LoggerService } from "../logger.service";
import { DataservicesConstants } from "../../dataservices/shared/dataservices-constants";
import { BsModalService } from "ngx-bootstrap/modal";
import { VerticalNavigationItem } from "patternfly-ng/navigation/vertical-navigation/vertical-navigation-item";

@Component({
  encapsulation: ViewEncapsulation.None,
  selector: "btl-vertical-nav",
  templateUrl: "./vertical-nav.component.html",
  styleUrls: ["./vertical-nav.component.css"]
})

export class VerticalNavComponent implements OnInit {

  public navigationItems: VerticalNavigationItem[];

  private logger: LoggerService;
  private modalService: BsModalService;
  private router: Router;

  constructor( router: Router,
               logger: LoggerService,
               modalService: BsModalService ) {
    this.router = router;
    this.logger = logger;
    this.modalService = modalService;
  }

  // public closeAbout( $event: AboutEvent ): void {
  //   this.aboutRef.hide();
  // }

  public ngOnInit(): void {
    // uncomment to debug router events
    // this.router.events.subscribe((event) => {
    //   console.error( event );
    // });

    this.navigationItems = [ DataservicesConstants.dataservicesNavItem, ConnectionsConstants.connectionsNavItem ];
  }

  public onNavigation( $event: VerticalNavigationItem ): void {
    const link: string[] = [ $event.url ];
    this.router.navigate( link ).then(() => {
      // nothing to do
    });
  }

  // public openAbout( template: TemplateRef< any > ): void {
  //   this.aboutRef = this.modalService.show( template );
  // }

}
