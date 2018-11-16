import { async, ComponentFixture, TestBed } from "@angular/core/testing";
import { FormsModule } from "@angular/forms";
import { RouterTestingModule } from "@angular/router/testing";
import { LoggerService } from "../../core/logger.service";
import { DataserviceService } from "../shared/dataservice.service";
import { DataservicesDetailsComponent } from "./dataservices-details.component";
import { DataservicesListComponent } from "./dataservices-list.component";
import { ViewsContentComponent } from "./views-content.component";
import { SharedModule } from "../../shared/shared.module";
import { MockVdbService } from "../shared/mock-vdb.service";
import { VdbService } from "../shared/vdb.service";
import { AppSettingsService } from "../../core/app-settings.service";
import { MockAppSettingsService } from "../../core/mock-app-settings.service";
import { NotifierService } from "../shared/notifier.service";
import {
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  NotificationModule,
  SortModule,
  TableModule,
  WizardModule } from "patternfly-ng";
import { TooltipModule } from 'ngx-bootstrap';
import { CodemirrorModule } from "ng2-codemirror";
import { HttpModule } from "@angular/http";

describe("DataservicesListComponent", () => {
  let component: DataservicesListComponent;
  let fixture: ComponentFixture<DataservicesListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        TooltipModule,
        CodemirrorModule,
        FormsModule,
        HttpModule,
        SharedModule,
        ActionModule,
        CardModule,
        EmptyStateModule,
        FilterModule,
        ListModule,
        NotificationModule,
        SortModule,
        TableModule,
        WizardModule
      ],
      declarations: [ DataservicesDetailsComponent, DataservicesListComponent, ViewsContentComponent ],
      providers: [
        LoggerService, DataserviceService, NotifierService,
        { provide: VdbService, useClass: MockVdbService },
        { provide: AppSettingsService, useClass: MockAppSettingsService }
      ]
    })
    .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DataservicesListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should be created", () => {
    console.log("========== [DataservicesListComponent] should be created");
    expect(component).toBeTruthy();
  });
});
