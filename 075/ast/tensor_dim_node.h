#pragma once

#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing tensor dim nodes.
   */
   class tensor_dim_node: public cdk::expression_node {
    cdk::expression_node *_base;
    cdk::expression_node *_position;

  public:
    tensor_dim_node(int lineno, cdk::expression_node *base, cdk::expression_node *position) :
        cdk::expression_node(lineno), _base(base), _position(position) {
    }

  public:
    cdk::expression_node *base() {
      return _base;
    }
    cdk::expression_node *position() {
      return _position;
    }


  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_dim_node(this, level);
    }

  };
} // udf