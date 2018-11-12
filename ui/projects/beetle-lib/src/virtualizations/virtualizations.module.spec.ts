import { VirtualizationsModule } from './virtualizations.module';

describe('VirtualizationsModule', () => {
  let virtualizationsModule: VirtualizationsModule;

  beforeEach(() => {
    virtualizationsModule = new VirtualizationsModule();
  });

  it('should create an instance', () => {
    expect(virtualizationsModule).toBeTruthy();
  });
});
