#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing tensor capacity nodes.
   */
  class tensor_capacity_node: public cdk::expression_node {
    cdk::expression_node *_base;

  public:
    tensor_capacity_node(int lineno, cdk::expression_node *base) :
        cdk::expression_node(lineno), _base(base) {
    }

  public:
    cdk::expression_node *base() {
      return _base;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_capacity_node(this, level);
    }

  };

} // udf