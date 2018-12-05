import { ModuleWithProviders } from "@angular/core";
import { ComponentFixture, TestBed } from "@angular/core/testing";
import { FormsModule } from "@angular/forms";
import { HttpModule } from "@angular/http";
import { By } from "@angular/platform-browser";
import { RouterTestingModule } from "@angular/router/testing";
import { ConnectionService } from "../connections/shared/connection.service";
import { MockConnectionService } from "../connections/shared/mock-connection.service";
import { AppSettingsService } from "../core/app-settings.service";
import { CoreModule } from "../core/core.module";
import { MockAppSettingsService } from "../core/mock-app-settings.service";
import { DataserviceCardComponent } from "./dataservices-cards/dataservice-card/dataservice-card.component";
import { DataservicesCardsComponent } from "./dataservices-cards/dataservices-cards.component";
import { DataservicesDetailsComponent } from "./dataservices-list/dataservices-details.component";
import { DataservicesListComponent } from "./dataservices-list/dataservices-list.component";
import { ViewsContentComponent } from "./dataservices-list/views-content.component";
import { DataservicesComponent } from "./dataservices.component";
import { DataserviceService } from "./shared/dataservice.service";
import { MockDataserviceService } from "./shared/mock-dataservice.service";
import { MockVdbService } from "./shared/mock-vdb.service";
import { NotifierService } from "./shared/notifier.service";
import { VdbService } from "./shared/vdb.service";
import { SqlControlComponent } from "./sql-control/sql-control.component";
import { OdataControlComponent } from "./odata-control/odata-control.component";
import { SharedModule } from "../shared/shared.module";
import { CodemirrorModule } from "ng2-codemirror";
import { ModalModule } from "ngx-bootstrap";
import {
  AboutModalModule,
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  SortModule,
  TableModule,
  ToastNotificationModule,
  WizardModule
} from "patternfly-ng";
import { AccordionModule } from 'ngx-bootstrap';
import { TooltipModule } from 'ngx-bootstrap';
import { SelectionService } from "../core/selection.service";

export const moduleRoot: ModuleWithProviders<any> = ModalModule.forRoot();
export const accordionModule: ModuleWithProviders<any> = AccordionModule.forRoot();
export const tooltipModule: ModuleWithProviders<any> = TooltipModule.forRoot();

describe("DataservicesComponent", () => {
  let component: DataservicesComponent;
  let fixture: ComponentFixture<DataservicesComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        CoreModule,
        FormsModule,
        HttpModule,
        moduleRoot,
        RouterTestingModule,
        SharedModule,
        CodemirrorModule,
        accordionModule,
        tooltipModule,
        AboutModalModule,
        ActionModule,
        CardModule,
        EmptyStateModule,
        FilterModule,
        ListModule,
        SortModule,
        TableModule,
        ToastNotificationModule,
        WizardModule
      ],
      declarations: [ DataservicesDetailsComponent,
                      DataservicesComponent,
                      DataservicesListComponent,
                      DataservicesCardsComponent,
                      DataserviceCardComponent,
                      SqlControlComponent,
                      OdataControlComponent,
                      ViewsContentComponent ],
      providers: [
        NotifierService,
        SelectionService,
        SelectionService,
        { provide: AppSettingsService, useClass: MockAppSettingsService },
        { provide: ConnectionService, useClass: MockConnectionService },
        { provide: DataserviceService, useClass: MockDataserviceService },
        { provide: VdbService, useClass: MockVdbService }
      ]
    });

    fixture = TestBed.createComponent(DataservicesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should be created", () => {
    console.log("========== [DataservicesComponent] should be created");
    expect(component).toBeTruthy();
  });

  it("should have Data Virtualizations Title", () => {
    console.log("========== [DataservicesComponent] should have Data Virtualizations Title");
    // query for the title <h2> by CSS element selector
    const de = fixture.debugElement.query(By.css("h2"));
    const el = de.nativeElement;
    expect(el.textContent).toEqual("Data Virtualizations");
  });

  it("should have Toolbar", () => {
    console.log("========== [DataservicesComponent] should have Toolbar");
    // query for the toolbar by css classname
    const de = fixture.debugElement.query(By.css(".toolbar-pf"));
    expect(de).toBeDefined();
  });

  it("should have Dataservices", () => {
    console.log("========== [DataservicesComponent] should have Dataservices");
    // Check component object
    const dataservices = component.allDataservices;
    expect(dataservices.length).toEqual(3);

    // Check html has the same number of dataservice cards
    const cardDebugElems = fixture.debugElement.queryAll(By.css(".object-card"));
    expect(cardDebugElems).toBeDefined();
    expect(cardDebugElems.length).toEqual(3);
  });

  it("should have initial card layout", () => {
    console.log("========== [DataservicesComponent] should have initial card layout");
    // btl-dataservices-cards should be present
    let debugEl = fixture.debugElement.query(By.css("btl-dataservices-cards"));
    const element = debugEl.nativeElement;
    expect(element).toBeDefined();

    // btl-dataservices-list should not be present
    debugEl = fixture.debugElement.query(By.css("btl-dataservices-list"));
    expect(debugEl).toBeNull();
  });

  // it("should toggle layout", () => {
  //   console.log("========== [DataservicesComponent] should toggle layout");
  //   // Initial layout should be Card Layout
  //   let cardDebugElem = fixture.debugElement.query(By.css("btl-dataservices-cards"));
  //   let listDebugElem = fixture.debugElement.query(By.css("btl-dataservices-list"));
  //   expect(cardDebugElem).toBeDefined();
  //   expect(listDebugElem).toBeNull();
  //   const cardElem = cardDebugElem.nativeElement;
  //   expect(cardElem).toBeDefined();
  //
  //   // Change the layout to ListLayout
  //   component.setListLayout();
  //   fixture.detectChanges();
  //
  //   // Verify that the layout has changed
  //   cardDebugElem = fixture.debugElement.query(By.css("btl-dataservices-cards"));
  //   listDebugElem = fixture.debugElement.query(By.css("app-dataservices-list"));
  //   expect(cardDebugElem).toBeNull();
  //   expect(listDebugElem).toBeDefined();
  //   const listElem = listDebugElem.nativeElement;
  //   expect(listElem).toBeDefined();
  // });

  // it("should filter dataservices", () => {
  //   console.log("========== [DataservicesComponent] should filter dataservices");
  //   // Expect 3 dataservices initially.
  //   let dataservices = component.filteredDataservices;
  //   expect(dataservices.length).toEqual(3);
  //
  //   // Set a name filter which satisfies none of the dataservices
  //   component.nameFilter = "g";
  //   component.filterDataservices();
  //   fixture.detectChanges();
  //
  //   // Now expect 0 services match
  //   dataservices = component.filteredDataservices;
  //   expect(dataservices.length).toEqual(0);
  // });

});
