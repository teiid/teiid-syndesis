import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BeetleLibComponent } from './beetle-lib.component';

describe('BeetleLibComponent', () => {
  let component: BeetleLibComponent;
  let fixture: ComponentFixture<BeetleLibComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ BeetleLibComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BeetleLibComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
