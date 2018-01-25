theory chatappdtrpls_CrdtProgram_WP_parameter_sendMessage_1
imports Why3
begin

why3_open "chatappdtrpls_CrdtProgram_WP_parameter_sendMessage_1.xml"

why3_vc WP_parameter_sendMessage
proof (auto simp add: message_exists_def)

  have "distinct [us,us1,us2,us3]"
    by (smt H46 H55 H60 H67 H68 H75 H76 H83 distinct_length_2_or_more distinct_one fun_upd_apply operation.distinct(9))

  hence us_not_eq[simp]: "us \<noteq> us1"
        "us \<noteq> us2"
        "us \<noteq> us3"
        "us1 \<noteq> us"
        "us1 \<noteq> us2"
        "us1 \<noteq> us3"
        "us2 \<noteq> us"
        "us2 \<noteq> us1"
        "us2 \<noteq> us3"
        "us3 \<noteq> us"
        "us3 \<noteq> us1"
        "us3 \<noteq> us2"
    by auto

  have [simp]: "state_visibleCalls5 (CallId us)"
    "state_visibleCalls5 (CallId us1)"
    "state_visibleCalls5 (CallId us2)"
    "state_visibleCalls5 (CallId us3)"
    by (auto simp add: H62 H70 H78 H86)



  show " \<exists>c1. (state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_author_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_content_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 c1 c2)"
  proof (cases "o1 = m")
    case True

    have us2_op: "state_callOps5 (CallId us2) = Op_message_chat_assign m toC"
      using H76 H83 H84 True by auto

    have us2_vis: "state_visibleCalls5 (CallId us2)"
      by (simp add: H78 H86)

    show " \<exists>c1. (state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_author_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_content_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 c1 c2)"
    proof 
      show "  (state_visibleCalls5 (CallId us2) \<and> (\<exists>args. state_callOps5 (CallId us2) = Op_message_author_assign m args) \<or>
          state_visibleCalls5 (CallId us2) \<and> (\<exists>args. state_callOps5 (CallId us2) = Op_message_content_assign m args) \<or>
          state_visibleCalls5 (CallId us2) \<and> (\<exists>args. state_callOps5 (CallId us2) = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 (CallId us2) c2)"
        apply (auto simp add: us2_op us2_vis)
        by (metis H77 H84 H85 H86 fun_upd_def operation.distinct(43))

    qed
      next
        case False

        have c_contains_m_new:
    "chat_messages_contains c m \<lparr>contents = state_locallyGenerated_messageId(o1 := True)\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr>
     \<lparr>contents = state_callOps5\<rparr> \<lparr>contents = state_visibleCalls5\<rparr> \<lparr>contents = state_happensBefore5\<rparr> \<lparr>contents = state_sameTransaction5\<rparr> \<lparr>contents = state_currentTransaction5\<rparr>
     \<lparr>contents = state_origin5\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
          using H91 by blast

        from c_contains_m_new
        have c_contains_m_old:
            "chat_messages_contains c m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr>
           \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
           \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
        proof (auto simp add: chat_messages_contains_def)
          fix c1 
          assume a0: "state_visibleCalls5 c1" 
            and a1: "state_callOps5 c1 = Op_chat_messages_add c m"
            and a2: "\<forall>d. state_visibleCalls5 d \<and> state_callOps5 d = Op_chat_delete c \<longrightarrow> state_happensBefore5 c1 d"
            and a3: "\<forall>c2. state_visibleCalls5 c2 \<and>
                state_callOps5 c2 = Op_chat_messages_remove c m \<and> (\<forall>d. state_visibleCalls5 d \<and> state_callOps5 d = Op_chat_delete c \<longrightarrow> state_happensBefore5 c2 d) \<longrightarrow>
                state_happensBefore5 c1 c2"

          have c1_not_eq0: "c1 \<noteq> (CallId us)"
            using H60 H68 H76 H84 a1 by auto

          have c1_not_eq1:"c1 \<noteq> (CallId us1)"
            using H60 H68 H76 H84 a1 by auto

          have c1_not_eq2:"c1 \<noteq> (CallId us2)"
            using H60 H68 H76 H84 a1 by auto

          have c1_not_eq3:"c1 \<noteq> (CallId us3)"
            using False H84 a1 by auto



          have "state_visibleCalls1 c1"
            using H62 H70 H78 H86 a0 c1_not_eq0 c1_not_eq1 c1_not_eq2 c1_not_eq3 by auto


          moreover have "state_callOps1 c1 = Op_chat_messages_add c m"
            using H60 H68 H76 H84 a1 c1_not_eq0 c1_not_eq1 c1_not_eq2 c1_not_eq3 by auto

          moreover have " (\<forall>d. state_visibleCalls1 d \<and> state_callOps1 d = Op_chat_delete c \<longrightarrow> state_happensBefore1 c1 d)"
            using H59 H60 H61 H62 H67 H68 H69 H70 H75 H76 H77 H78 H83 H84 H85 H86 a2 c1_not_eq0 c1_not_eq1 c1_not_eq2 c1_not_eq3 by fastforce

          moreover have "(\<forall>c2. state_visibleCalls1 c2 \<and>
                         state_callOps1 c2 = Op_chat_messages_remove c m \<and> (\<forall>d. state_visibleCalls1 d \<and> state_callOps1 d = Op_chat_delete c \<longrightarrow> state_happensBefore1 c2 d) \<longrightarrow>
                         state_happensBefore1 c1 c2)"
            using a3 apply (auto simp add: H59 H60 H61 H62 H67 H68 H69 H70 H75 H76 H77 H78 H83 H84 H85 H86 a2 c1_not_eq0 c1_not_eq1 c1_not_eq2 c1_not_eq3)
            using [[smt_solver=cvc4]]
            by (smt H59 H60 H67 H68 H75 H76 H83 Select_eq Select_neq operation.distinct(3) operation.distinct(35) operation.distinct(91) operation.distinct(93) operation.distinct(95))

          ultimately
          have "state_visibleCalls1 c1 \<and>
                   state_callOps1 c1 = Op_chat_messages_add c m \<and>
                   (\<forall>d. state_visibleCalls1 d \<and> state_callOps1 d = Op_chat_delete c \<longrightarrow> state_happensBefore1 c1 d) \<and>
                   (\<forall>c2. state_visibleCalls1 c2 \<and>
                         state_callOps1 c2 = Op_chat_messages_remove c m \<and> (\<forall>d. state_visibleCalls1 d \<and> state_callOps1 d = Op_chat_delete c \<longrightarrow> state_happensBefore1 c2 d) \<longrightarrow>
                         state_happensBefore1 c1 c2)"
            by metis
          from this
          show "\<exists>c1. state_visibleCalls1 c1 \<and>
                   state_callOps1 c1 = Op_chat_messages_add c m \<and>
                   (\<forall>d. state_visibleCalls1 d \<and> state_callOps1 d = Op_chat_delete c \<longrightarrow> state_happensBefore1 c1 d) \<and>
                   (\<forall>c2. state_visibleCalls1 c2 \<and>
                         state_callOps1 c2 = Op_chat_messages_remove c m \<and> (\<forall>d. state_visibleCalls1 d \<and> state_callOps1 d = Op_chat_delete c \<longrightarrow> state_happensBefore1 c2 d) \<longrightarrow>
                         state_happensBefore1 c1 c2)"
            by auto
        qed

        have "chat_messages_contains c m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr>
           \<lparr>contents = state_callOps1\<rparr> \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr>
           \<lparr>contents = state_origin1\<rparr> \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>
 \<longrightarrow>
          message_exists m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr> \<lparr>contents = state_callOps1\<rparr>
           \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr> \<lparr>contents = state_origin1\<rparr>
           \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
          using H31 by simp

        with c_contains_m_old
        have message_exists_old: 
          "message_exists m \<lparr>contents = state_locallyGenerated_messageId\<rparr> \<lparr>contents = state_exposed_messageId\<rparr> \<lparr>contents = state_knownIds_messageId\<rparr> \<lparr>contents = state_callOps1\<rparr>
           \<lparr>contents = state_visibleCalls1\<rparr> \<lparr>contents = state_happensBefore1\<rparr> \<lparr>contents = state_sameTransaction1\<rparr> \<lparr>contents = state_currentTransaction1\<rparr> \<lparr>contents = state_origin1\<rparr>
           \<lparr>contents = state_invocations2\<rparr> \<lparr>contents = state_invocationResult1\<rparr> \<lparr>contents = state_invocationHappensBefore1\<rparr>"
          by auto

        from this 
        obtain c1 where 
              b1: "state_visibleCalls1 c1"
          and b2: "(\<exists>args. state_callOps1 c1 = Op_message_author_assign m args) 
             \<or> (\<exists>args. state_callOps1 c1 = Op_message_content_assign m args)
             \<or> (\<exists>args. state_callOps1 c1 = Op_message_chat_assign m args)"
          and b3: "\<forall>c2. state_visibleCalls1 c2 \<and> state_callOps1 c2 = Op_message_delete m \<longrightarrow> state_happensBefore1 c1 c2"
          by (auto simp add: message_exists_def)


        from b1
        have h1: "state_visibleCalls5 c1"
          by (simp add: H62 H70 H78 H86)


        have "(state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_author_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_content_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 c1 c2)"
          apply (auto simp add: h1)
          sorry
        from this
        show "\<exists>c1. (state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_author_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_content_assign m args) \<or>
          state_visibleCalls5 c1 \<and> (\<exists>args. state_callOps5 c1 = Op_message_chat_assign m args)) \<and>
         (\<forall>c2. state_visibleCalls5 c2 \<and> state_callOps5 c2 = Op_message_delete m \<longrightarrow> state_happensBefore5 c1 c2)"
          by metis


        find_theorems chat_messages_contains

        then show ?thesis sorry
      qed
      find_theorems message_exists
why3_end

end

end