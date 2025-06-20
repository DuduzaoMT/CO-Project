#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing tensor rank nodes.
   */
  class tensor_rank_node: public cdk::expression_node {
    cdk::expression_node *_base;

  public:
    tensor_rank_node(int lineno, cdk::expression_node *base) :
        cdk::expression_node(lineno), _base(base) {
    }

  public:
    cdk::expression_node *base() {
      return _base;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_rank_node(this, level);
    }

  };

} // udf