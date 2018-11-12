import { TestBed } from '@angular/core/testing';

import { BeetleLibService } from './beetle-lib.service';

describe('BeetleLibService', () => {
  beforeEach(() => TestBed.configureTestingModule({}));

  it('should be created', () => {
    const service: BeetleLibService = TestBed.get(BeetleLibService);
    expect(service).toBeTruthy();
  });
});
