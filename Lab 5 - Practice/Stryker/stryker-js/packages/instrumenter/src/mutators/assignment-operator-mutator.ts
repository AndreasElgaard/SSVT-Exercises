import { type types as t } from '@babel/core';

import { NodeMutator } from './index.js';

export const assignmentOperatorMutator: NodeMutator = {
  name: 'AssignmentOperator',

  *mutate(path) {
    // TODO!
  },
};
