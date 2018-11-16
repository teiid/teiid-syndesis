import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpModule } from "@angular/http";
import { FormsModule } from "@angular/forms";
import { RouterTestingModule } from "@angular/router/testing";
import { LoggerService } from "../../../../core/logger.service";
import { MockAppSettingsService } from "../../../../core/mock-app-settings.service";
import { AppSettingsService } from "../../../../core/app-settings.service";
import { ViewEditorService } from "../view-editor.service";
import { ViewEditorHeaderComponent } from "./view-editor-header.component";
import { VdbService } from "../../../shared/vdb.service";
import { MockVdbService } from "../../../shared/mock-vdb.service";
import { NotifierService } from "../../../shared/notifier.service";
import { DataserviceService } from "../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../shared/mock-dataservice.service";
import { TableModule } from "patternfly-ng";
import { SelectionService } from "../../../../core/selection.service";
import { BsModalService } from "ngx-bootstrap";
import { Dataservice } from "../../../shared/dataservice.model";

describe('ViewEditorHeaderComponent', () => {
  let component: ViewEditorHeaderComponent;
  let fixture: ComponentFixture<ViewEditorHeaderComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        FormsModule,
        HttpModule,
        RouterTestingModule,
        TableModule
      ],
      declarations: [ ViewEditorHeaderComponent ],
      providers: [
        BsModalService,
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        { provide: DataserviceService, useClass: MockDataserviceService },
        LoggerService,
        NotifierService,
        SelectionService,
        { provide: VdbService, useClass: MockVdbService },
        ViewEditorService
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ViewEditorHeaderComponent);
    component = fixture.componentInstance;

    const selService = TestBed.get( SelectionService );
    const ds: Dataservice = new Dataservice();
    ds.setId("testDs");
    ds.setServiceVdbName("testDsVdb");
    // noinspection JSUnusedAssignment
    selService.setSelectedVirtualization( ds );

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
