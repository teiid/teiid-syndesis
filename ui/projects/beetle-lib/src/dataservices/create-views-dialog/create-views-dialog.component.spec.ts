import { async, ComponentFixture, TestBed } from "@angular/core/testing";
import { ModuleWithProviders } from "@angular/core";

import { CreateViewsDialogComponent } from "./create-views-dialog.component";
import { HttpModule } from "@angular/http";
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
import { BsModalRef, ModalModule } from "ngx-bootstrap";
import { ConnectionService } from "../../connections/shared/connection.service";
import { MockConnectionService } from "../../connections/shared/mock-connection.service";
import { AppSettingsService } from "../../core/app-settings.service";
import { LoggerService } from "../../core/logger.service";
import { NotifierService } from "../shared/notifier.service";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { DataserviceService } from "../shared/dataservice.service";
import { MockDataserviceService } from "../shared/mock-dataservice.service";
import { VdbService } from "../shared/vdb.service";
import { MockVdbService } from "../shared/mock-vdb.service";

export const moduleRoot: ModuleWithProviders<any> = ModalModule.forRoot();

describe("CreateViewsDialogComponent", () => {
  let component: CreateViewsDialogComponent;
  let fixture: ComponentFixture<CreateViewsDialogComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule,
        FormsModule,
        ReactiveFormsModule,
        moduleRoot,
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
      declarations: [ CreateViewsDialogComponent ],
      providers: [ AppSettingsService, BsModalRef, LoggerService, NotifierService,
        { provide: ConnectionService, useClass: MockConnectionService },
        { provide: DataserviceService, useClass: MockDataserviceService },
        { provide: VdbService, useClass: MockVdbService }
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateViewsDialogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should be created", () => {
    expect(component).toBeTruthy();
  });
});
