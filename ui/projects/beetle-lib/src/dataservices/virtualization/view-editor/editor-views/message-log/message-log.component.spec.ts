import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from "@angular/http";
import { MockAppSettingsService } from "../../../../../core/mock-app-settings.service";
import { AppSettingsService } from "../../../../../core/app-settings.service";
import {
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  SortModule,
  TableModule,
  WizardModule } from "patternfly-ng";
import { LoggerService } from "../../../../../core/logger.service";
import { DataserviceService } from "../../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../../shared/mock-dataservice.service";
import { VdbService } from "../../../../shared/vdb.service";
import { MockVdbService } from "../../../../shared/mock-vdb.service";
import { NotifierService } from "../../../../shared/notifier.service";
import { ViewEditorService } from "../../view-editor.service";
import { MessageLogComponent } from "./message-log.component";
import { SelectionService } from "../../../../../core/selection.service";

describe('MessageLogComponent', () => {
  let component: MessageLogComponent;
  let fixture: ComponentFixture<MessageLogComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ActionModule,
        CardModule,
        EmptyStateModule,
        FilterModule,
        ListModule,
        SortModule,
        TableModule,
        WizardModule,
        HttpModule
      ],
      declarations: [ MessageLogComponent ],
      providers: [
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        { provide: DataserviceService, useClass: MockDataserviceService },
        LoggerService,
        NotifierService,
        SelectionService,
        { provide: VdbService, useClass: MockVdbService },
        ViewEditorService
      ]
    })
    .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MessageLogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
