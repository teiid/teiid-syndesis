import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { LibContentComponent } from './lib-content.component';
import { RouterTestingModule } from "@angular/router/testing";

describe('LibContentComponent', () => {
  let component: LibContentComponent;
  let fixture: ComponentFixture<LibContentComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ RouterTestingModule ],
      declarations: [ LibContentComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LibContentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
