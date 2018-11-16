import { Composition } from "./composition.model";
import { CompositionType } from "./composition-type.enum";
import { CompositionOperator } from "./composition-operator.enum";

describe("Composition", () => {
  let composition: Composition;

  beforeEach(() => {
    composition = null;
  });

  it("should create", () => {
    console.log("========== [Composition] should create");
    composition = Composition.create(
      {
        "name": "myView",
        "leftSourcePath": "leftPath",
        "rightSourcePath": "rightPath",
        "type": "INNER_JOIN",
        "condition": "EQ"
      }
    );

    expect(composition.getName()).toEqual("myView");
    expect(composition.getLeftSourcePath()).toEqual("leftPath");
    expect(composition.getRightSourcePath()).toEqual("rightPath");
    expect(composition.getType()).toEqual(CompositionType.INNER_JOIN);
    expect(composition.getOperator()).toEqual(CompositionOperator.EQ);
  });

});
