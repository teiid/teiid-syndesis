import { ConnectionsModule } from './connections.module';

describe('ConnectionsModule', () => {
  let connectionsModule: ConnectionsModule;

  beforeEach(() => {
    connectionsModule = new ConnectionsModule();
  });

  it('should create an instance', () => {
    expect(connectionsModule).toBeTruthy();
  });
});
