import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { VirtualizationsComponent } from './virtualizations.component';

describe('VirtualizationsComponent', () => {
  let component: VirtualizationsComponent;
  let fixture: ComponentFixture<VirtualizationsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ VirtualizationsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VirtualizationsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
