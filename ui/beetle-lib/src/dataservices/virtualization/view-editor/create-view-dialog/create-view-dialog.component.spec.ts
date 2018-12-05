import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ModuleWithProviders } from "@angular/core";

import { CreateViewDialogComponent } from './create-view-dialog.component';
import { HttpModule } from "@angular/http";
import { BsModalRef, ModalModule } from "ngx-bootstrap";
import {
  ActionModule
} from "patternfly-ng";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { VdbService } from "../../../shared/vdb.service";
import { MockVdbService } from "../../../shared/mock-vdb.service";
import { AppSettingsService } from "../../../../core/app-settings.service";
import { LoggerService } from "../../../../core/logger.service";
import { NotifierService } from "../../../shared/notifier.service";
import { SelectionService } from "../../../../core/selection.service";

export const moduleRoot: ModuleWithProviders<any> = ModalModule.forRoot();

describe('CreateViewDialogComponent', () => {
  let component: CreateViewDialogComponent;
  let fixture: ComponentFixture<CreateViewDialogComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpModule,
        FormsModule,
        ReactiveFormsModule,
        moduleRoot,
        ActionModule
      ],
      declarations: [ CreateViewDialogComponent ],
      providers: [ AppSettingsService, BsModalRef, LoggerService, NotifierService, SelectionService,
        { provide: VdbService, useClass: MockVdbService }
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateViewDialogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
