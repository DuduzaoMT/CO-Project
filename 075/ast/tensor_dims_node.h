#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing tensor dims nodes.
   */
  class tensor_dims_node: public cdk::expression_node {
    cdk::expression_node *_base;

  public:
    tensor_dims_node(int lineno, cdk::expression_node *base) :
        cdk::expression_node(lineno), _base(base) {
    }

  public:
    cdk::expression_node *base() {
      return _base;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_dims_node(this, level);
    }

  };

} // udf