#pragma once

#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing tensor nodes.
   */
  class tensor_node: public cdk::expression_node {
    cdk::sequence_node *_fields;
    cdk::sequence_node *_expanded_fields = nullptr;

  public:
    inline tensor_node(int lineno, cdk::sequence_node *fields) :
        cdk::expression_node(lineno), _fields(fields) {
    }

  public:
    inline cdk::sequence_node* fields() {
        return _fields;
    }
    inline cdk::expression_node* field(size_t ix) {
      return (cdk::expression_node*)_fields->node(ix);
    }
    inline cdk::sequence_node* expanded_fields() {
      return _expanded_fields;
    }
    inline void expanded_fields(cdk::sequence_node *seq) {
      _expanded_fields = seq;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_node(this, level);
    }

  };

} // udf
