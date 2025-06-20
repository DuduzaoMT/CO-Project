#pragma once

#include <cdk/ast/binary_operation_node.h>

namespace udf {

  /**
   * Class for describing tensor contract nodes.
   */
  class tensor_contract_node: public cdk::binary_operation_node {

  public:
    tensor_contract_node(int lineno, cdk::expression_node *left, cdk::expression_node *right) :
        cdk::binary_operation_node(lineno, left, right) {
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_contract_node(this, level);
    }

  };

} // udf