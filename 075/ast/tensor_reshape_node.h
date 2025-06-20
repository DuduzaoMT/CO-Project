#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  /**
   * Class for describing tensor reshape nodes.
   */
  class tensor_reshape_node: public cdk::expression_node {
    cdk::expression_node *_base;
    cdk::sequence_node *_sizes;

  public:
    tensor_reshape_node(int lineno, cdk::expression_node *base, cdk::sequence_node *sizes) :
        cdk::expression_node(lineno), _base(base), _sizes(sizes) {
    }

  public:
    cdk::expression_node* size(size_t ix) {
      return (cdk::expression_node*)_sizes->node(ix);
    }

    cdk::expression_node *base() {
      return _base;
    }

    cdk::sequence_node *sizes() {
      return _sizes;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_reshape_node(this, level);
    }

  };

} // udf