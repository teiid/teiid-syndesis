import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ProjectedColumnsEditorComponent } from './projected-columns-editor.component';
import { TableModule } from "patternfly-ng";
import { LoggerService } from "../../../../../core/logger.service";
import { SelectionService } from "../../../../../core/selection.service";
import { AppSettingsService } from "../../../../../core/app-settings.service";
import { MockAppSettingsService } from "../../../../../core/mock-app-settings.service";
import { DataserviceService } from "../../../../shared/dataservice.service";
import { MockDataserviceService } from "../../../../shared/mock-dataservice.service";
import { NotifierService } from "../../../../shared/notifier.service";
import { VdbService } from "../../../../shared/vdb.service";
import { MockVdbService } from "../../../../shared/mock-vdb.service";
import { ViewEditorService } from "../../../../virtualization/view-editor/view-editor.service";
import { HttpModule } from "@angular/http";

describe('ProjectedColumnsEditorComponent', () => {
  let component: ProjectedColumnsEditorComponent;
  let fixture: ComponentFixture<ProjectedColumnsEditorComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule,
        TableModule
      ],
      declarations: [ ProjectedColumnsEditorComponent ],
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
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectedColumnsEditorComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
