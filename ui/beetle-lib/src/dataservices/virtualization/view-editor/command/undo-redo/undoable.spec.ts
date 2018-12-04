import { Undoable } from "./undoable";
import { UpdateViewNameCommand } from "../update-view-name-command";
import { CommandFactory } from "../command-factory";

describe("Undoable", () => {
  let result: Undoable | Error;

  beforeEach(() => {
    result = null;
  });

  it("should create", () => {
    console.log( "========== [Undoable] should create" );
    result = CommandFactory.decodeUndoable(
      {
        "undo": {
          "id": "UpdateViewNameCommand",
          "args": { "oldName": "v", "newName": "" }
        },
        "redo": {
          "id": "UpdateViewNameCommand",
          "args": { "oldName": "b", "newName": "v" }
        }
      }
    );

    if ( result instanceof Undoable ) {
      const undoable = result as Undoable;
      expect( undoable.undoCommand.id ).toEqual( UpdateViewNameCommand.id );
      expect( undoable.redoCommand.id ).toEqual( UpdateViewNameCommand.id );
    } else {
      fail("Unable to create Undoable");
    }
  });

});
