import { async, ComponentFixture, TestBed } from "@angular/core/testing";
import { RouterTestingModule } from "@angular/router/testing";
import { ConnectionDetailsComponent } from "./connection-details.component";
import { ConnectionsListComponent } from "./connections-list.component";
import {
  ActionModule,
  CardModule,
  EmptyStateModule,
  FilterModule,
  ListModule,
  SortModule,
  WizardModule
} from "patternfly-ng";

describe("ConnectionsListComponent", () => {
  let component: ConnectionsListComponent;
  let fixture: ComponentFixture<ConnectionsListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ActionModule,
        CardModule,
        EmptyStateModule,
        FilterModule,
        ListModule,
        SortModule,
        WizardModule
      ],
      declarations: [ ConnectionDetailsComponent, ConnectionsListComponent ]
    })
    .compileComponents().then(() => {
      // nothing to do
    });
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConnectionsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it("should be created", () => {
    console.log("========== [ConnectionsListComponent] should be created");
    expect(component).toBeTruthy();
  });
});
