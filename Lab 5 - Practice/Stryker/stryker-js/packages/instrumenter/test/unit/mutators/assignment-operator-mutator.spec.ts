import { expect } from 'chai';

import { assignmentOperatorMutator as sut } from '../../../src/mutators/assignment-operator-mutator.js';

describe(sut.name, () => {
  it('should have name "AssignmentOperator"', () => {
    expect(sut.name).eq('AssignmentOperator');
  });

  // TODO
});
