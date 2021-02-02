package repliss

import crdtver.Repliss.{Quickcheck, ReplissResult}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AnyValue, DataTypeValue, CallAction, InvariantCheck, InvariantViolationException, InvocationId, LocalAction, NewId, Return, StartTransaction, TransactionId, domainValue}
import crdtver.utils.Helper
import crdtver.{Repliss, RunArgs}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/**
  * Tests for the random test generator
  */
class InterpreterTests extends AnyFunSuite with Matchers {




//   test("find chatapp example") {

//     val prog = Repliss.parseAndTypecheck("chatapp", Helper.getResource("/examples/buggy/chatapp_fail1.rpls")).get()
//     val i = new Interpreter(prog, RunArgs(), domainSize = 3)
//     var s = Interpreter.State()
//     val i1 = InvocationId(1)
//     val i2 = InvocationId(2)
//     val i3 = InvocationId(3)
//     val i4 = InvocationId(4)
//     val i5 = InvocationId(5)
//     val t1 = TransactionId(1)
//     val t2 = TransactionId(2)
//     val t3 = TransactionId(3)
//     val t4 = TransactionId(4)

// //      [info]  0. invoc_1 call sendMessage(UserId_1, String_0)
// //      [info]  1. invoc_1    startTx() => tx_1
// //      [info]  2. invoc_1    newId(0)
// //      [info]  3. invoc_1    return
// //      [info]  4. invoc_2 call editMessage(MessageId_000, String_0)
// //      [info]  5. invoc_2    startTx() => tx_2
// //      [info]  6. invoc_2    return
// //      [info]  7. invoc_3 call deleteMessage(MessageId_000)
// //      [info]  8. invoc_3    startTx() => tx_3
// //      [info]  9. invoc_3    return
// //      [info]  10. invoc_4 call getMessage(MessageId_000)
// //      [info]  11. invoc_4    startTx(tx_2, tx_3) => tx_4
// //      [info]  12. invoc_4    return

//     val user1 = domainValue("UserId", 1)
//     val string0 = domainValue("String", 0)
//     val string1 = domainValue("String", 1)
//     val message1 =domainValue("MessageId", 1)

//     s = i.executeAction(s, CallAction(i1, "sendMessage", List(user1, string0))).get
//     s = i.executeAction(s, LocalAction(i1, StartTransaction(t1, Set()))).get
//     s = i.executeAction(s, LocalAction(i1, NewId(1))).get
//     s = i.executeAction(s, LocalAction(i1, Return())).get

//     println(s"knownIds = ${s.knownIds}")

//     for (k <- s.knownIds.values; kk <- k.keys) {
//       println(s"Id = $kk (${kk.value.getClass})")
//     }

//     s = i.executeAction(s, CallAction(i2, "editMessage", List(message1, string1))).get
//     s = i.executeAction(s, LocalAction(i2, StartTransaction(t2, Set()))).get
//     s = i.executeAction(s, LocalAction(i2, Return())).get

//     s = i.executeAction(s, CallAction(i3, "deleteMessage", List(message1))).get
//     s = i.executeAction(s, LocalAction(i3, StartTransaction(t3, Set()))).get
//     s = i.executeAction(s, LocalAction(i3, Return())).get

//     s = i.executeAction(s, CallAction(i4, "getMessage", List(message1))).get
//     s = i.executeAction(s, LocalAction(i4, StartTransaction(t4, Set(t2, t3)))).get
//     s = i.executeAction(s, LocalAction(i4, Return())).get

// //    s = i.executeAction(s, CallAction(i5, "getMessage", List(AnyValue("MessageId_001")))).get
//     try {
//       i.checkInvariants(s)
// //      s = i.executeAction(s, InvariantCheck(i5)).get
//       fail("Invariant-check should fail")
//     } catch {
//       case iv: InvariantViolationException =>
//         println(iv)
//         for (info <- iv.info)
//           println(info)
//     }

//   }


  test("file_system_fail1") {

    val prog = Repliss.parseAndTypecheck("fs", Helper.getResource("/examples/buggy/5.rpls")).get()
    val i = new Interpreter(prog, RunArgs(), domainSize = 3)
    var s = Interpreter.State()

    val createAdminInvocId = InvocationId(1);
    val createUserInvocId = InvocationId(2);
    val createGroupInvocId = InvocationId(3);
    val assignToGroupInvocId = InvocationId(4);
    val createFileInvocId = InvocationId(5);
    val changeOwnerInvocId = InvocationId(6);
    val changeOwnerPermsInvocId = InvocationId(7);
    val readFileInvocId = InvocationId(8);
    

    val createAdminTrasnactionId = TransactionId(1);
    val createUserTrasnactionId = TransactionId(2);
    val createGroupTransactionId = TransactionId(3);
    val assignToGroupTransactionId = TransactionId(4);
    val createFileTrasnactionId = TransactionId(5);
    val changeOwnerTrasnactionId = TransactionId(6);
    val changeOwnerPermsTrasnactionId = TransactionId(7);
    val readFileTrasnactionId = TransactionId(8);
    

    val admin1 = domainValue("UserId", 1);
    val user1 = domainValue("UserId", 2);
    val file1 = domainValue("NodeId", 1);
    val group1 = domainValue("GroupId", 1); 
    val writAccessRight = AnyValue(DataTypeValue("UW", List()));

    // create admin
    s = i.executeAction(s, CallAction(createAdminInvocId, "createUser", List(AnyValue(true)))).get // create admin
    s = i.executeAction(s, LocalAction(createAdminInvocId, StartTransaction(createAdminTrasnactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createAdminInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createAdminInvocId, Return())).get
    println(s"admin created");
    
    // create user
    s = i.executeAction(s, CallAction(createUserInvocId, "createUser", List(AnyValue(false)))).get // create admin
    s = i.executeAction(s, LocalAction(createUserInvocId, StartTransaction(createUserTrasnactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createUserInvocId, NewId(2))).get
    s = i.executeAction(s, LocalAction(createUserInvocId, Return())).get
    println(s"user created");
    
    // create group
    s = i.executeAction(s, CallAction(createGroupInvocId, "createGroup", List())).get // create admin
    s = i.executeAction(s, LocalAction(createGroupInvocId, StartTransaction(createGroupTransactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createGroupInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createGroupInvocId, Return())).get
    println("Group created")

    // assign user to group
    s = i.executeAction(s, CallAction(assignToGroupInvocId, "assignUserToGroup", List(admin1, group1, user1))).get
    s = i.executeAction(s, LocalAction(assignToGroupInvocId, StartTransaction(assignToGroupTransactionId, Set(createUserTrasnactionId, createAdminTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(assignToGroupInvocId, Return())).get
    println(s"user assigned to group");

    // create file
    s = i.executeAction(s, CallAction(createFileInvocId, "createFile", List(user1, group1))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, StartTransaction(createFileTrasnactionId, Set(assignToGroupTransactionId)))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, Return())).get
    println(s"file created");

    // change owner of file
    s = i.executeAction(s, CallAction(changeOwnerInvocId, "changeOwner", List(admin1, admin1, file1))).get
    s = i.executeAction(s, LocalAction(changeOwnerInvocId, StartTransaction(changeOwnerTrasnactionId, Set(createFileTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(changeOwnerInvocId, Return())).get
    println(s"owner changed");

    // change owner perms//changeOwnerPermission(userId: UserId,  newPermission: AccessRight, fileId: NodeId)
    s = i.executeAction(s, CallAction(changeOwnerPermsInvocId, "changeOwnerPermission", List(user1,  writAccessRight, file1))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId, StartTransaction(changeOwnerPermsTrasnactionId, Set(createFileTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId, Return())).get
    println(s"owner perms changed");

    // read
    s = i.executeAction(s, CallAction(readFileInvocId, "readFile", List(user1, file1))).get
    s = i.executeAction(s, LocalAction(readFileInvocId, StartTransaction(readFileTrasnactionId, Set(changeOwnerTrasnactionId, changeOwnerPermsTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(readFileInvocId, Return())).get
    println(s"done");

    // println(s"knownIds = ${s.knownIds}")

    // for (k <- s.knownIds.values; kk <- k.keys) {
    //   println(s"Id = $kk (${kk.value.getClass})")
    // }

    // s = i.executeAction(s, CallAction(i2, "editMessage", List(message1, string1))).get
    // s = i.executeAction(s, LocalAction(i2, StartTransaction(t2, Set()))).get
    // s = i.executeAction(s, LocalAction(i2, Return())).get

    // s = i.executeAction(s, CallAction(i3, "deleteMessage", List(message1))).get
    // s = i.executeAction(s, LocalAction(i3, StartTransaction(t3, Set()))).get
    // s = i.executeAction(s, LocalAction(i3, Return())).get

    // s = i.executeAction(s, CallAction(i4, "getMessage", List(message1))).get
    // s = i.executeAction(s, LocalAction(i4, StartTransaction(t4, Set(t2, t3)))).get
    // s = i.executeAction(s, LocalAction(i4, Return())).get

//    s = i.executeAction(s, CallAction(i5, "getMessage", List(AnyValue("MessageId_001")))).get
    try {
      i.checkInvariants(s)
//      s = i.executeAction(s, InvariantCheck(i5)).get
      fail("Invariant-check should fail")
    } catch {
      case iv: InvariantViolationException =>
        println(iv)
        for (info <- iv.info)
          println(info)
    }

  }

    test("file_system_pass1") {

    val prog = Repliss.parseAndTypecheck("fs", Helper.getResource("/examples/buggy/5.rpls")).get()
    val i = new Interpreter(prog, RunArgs(), domainSize = 3)
    var s = Interpreter.State()

    val createAdminInvocId = InvocationId(1);
    val createUserInvocId = InvocationId(2);
    val createGroupInvocId = InvocationId(3);
    val assignToGroupInvocId = InvocationId(4);
    val createFileInvocId = InvocationId(5);
    val changeOwnerInvocId = InvocationId(6);
    val changeOwnerPermsInvocId = InvocationId(7);
    val changeOwnerPermsInvocId2 = InvocationId(8);
    val readFileInvocId = InvocationId(9);
    

    val createAdminTrasnactionId = TransactionId(1);
    val createUserTrasnactionId = TransactionId(2);
    val createGroupTransactionId = TransactionId(3);
    val assignToGroupTransactionId = TransactionId(4);
    val createFileTrasnactionId = TransactionId(5);
    val changeOwnerTrasnactionId = TransactionId(6);
    val changeOwnerPermsTrasnactionId = TransactionId(7);
    val changeOwnerPermsTrasnactionId2 = TransactionId(8);
    val readFileTrasnactionId = TransactionId(9);
    

    val admin1 = domainValue("UserId", 1);
    val user1 = domainValue("UserId", 2);
    val file1 = domainValue("NodeId", 1);
    val group1 = domainValue("GroupId", 1); 
    val writeAccessRight = AnyValue(DataTypeValue("UW", List()));
    val readWriteAccessRight = AnyValue(DataTypeValue("URW", List()));

    // create admin
    s = i.executeAction(s, CallAction(createAdminInvocId, "createUser", List(AnyValue(true)))).get // create admin
    s = i.executeAction(s, LocalAction(createAdminInvocId, StartTransaction(createAdminTrasnactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createAdminInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createAdminInvocId, Return())).get
    println(s"admin created");
    
    // create user
    s = i.executeAction(s, CallAction(createUserInvocId, "createUser", List(AnyValue(false)))).get // create admin
    s = i.executeAction(s, LocalAction(createUserInvocId, StartTransaction(createUserTrasnactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createUserInvocId, NewId(2))).get
    s = i.executeAction(s, LocalAction(createUserInvocId, Return())).get
    println(s"user created");
    
    // create group
    s = i.executeAction(s, CallAction(createGroupInvocId, "createGroup", List())).get // create admin
    s = i.executeAction(s, LocalAction(createGroupInvocId, StartTransaction(createGroupTransactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createGroupInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createGroupInvocId, Return())).get
    println("Group created")

    // assign user to group
    s = i.executeAction(s, CallAction(assignToGroupInvocId, "assignUserToGroup", List(admin1, group1, user1))).get
    s = i.executeAction(s, LocalAction(assignToGroupInvocId, StartTransaction(assignToGroupTransactionId, Set(createUserTrasnactionId, createAdminTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(assignToGroupInvocId, Return())).get
    println(s"user assigned to group");

    // create file
    s = i.executeAction(s, CallAction(createFileInvocId, "createFile", List(user1, group1))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, StartTransaction(createFileTrasnactionId, Set(assignToGroupTransactionId)))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, Return())).get
    println(s"file created");

    // change owner of file
    s = i.executeAction(s, CallAction(changeOwnerInvocId, "changeOwner", List(admin1, admin1, file1))).get
    s = i.executeAction(s, LocalAction(changeOwnerInvocId, StartTransaction(changeOwnerTrasnactionId, Set(createFileTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(changeOwnerInvocId, Return())).get
    println(s"owner changed");

    // change owner perms
    s = i.executeAction(s, CallAction(changeOwnerPermsInvocId, "changeOwnerPermission", List(user1,  writeAccessRight, file1))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId, StartTransaction(changeOwnerPermsTrasnactionId, Set(createFileTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId, Return())).get
    println(s"owner perms changed");


     // change owner perms
    s = i.executeAction(s, CallAction(changeOwnerPermsInvocId2, "changeOwnerPermission", List(user1, readWriteAccessRight, file1))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId2, StartTransaction(changeOwnerPermsTrasnactionId2, Set(changeOwnerPermsTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId2, Return())).get
    println(s"owner perms changed 2");

    // read
    s = i.executeAction(s, CallAction(readFileInvocId, "readFile", List(user1, file1))).get
    s = i.executeAction(s, LocalAction(readFileInvocId, StartTransaction(readFileTrasnactionId, Set(changeOwnerTrasnactionId, changeOwnerPermsTrasnactionId2)))).get
    s = i.executeAction(s, LocalAction(readFileInvocId, Return())).get
    println("read")

   
    println(s"done");

    try {
      i.checkInvariants(s)
    } catch {
        case iv: InvariantViolationException =>
            fail("Invariant-check should pass")
            println(iv)
            for (info <- iv.info)
                println(info)
    }

  }

    test("file_system_pass2") {

    val prog = Repliss.parseAndTypecheck("fs", Helper.getResource("/examples/buggy/5.rpls")).get()
    val i = new Interpreter(prog, RunArgs(), domainSize = 3)
    var s = Interpreter.State()

    val createAdminInvocId = InvocationId(1);
    val createUserInvocId = InvocationId(2);
    val createGroupInvocId = InvocationId(3);
    val assignToGroupInvocId = InvocationId(4);
    val createFileInvocId = InvocationId(5);
    val changeOwnerPermsInvocId = InvocationId(6);
    val readFileInvocId = InvocationId(7);
    

    val createAdminTrasnactionId = TransactionId(1);
    val createUserTrasnactionId = TransactionId(2);
    val createGroupTransactionId = TransactionId(3);
    val assignToGroupTransactionId = TransactionId(4);
    val createFileTrasnactionId = TransactionId(5);
    val changeOwnerPermsTrasnactionId = TransactionId(6);
    val readFileTrasnactionId = TransactionId(7);
    

    val admin1 = domainValue("UserId", 1);
    val user1 = domainValue("UserId", 2);
    val file1 = domainValue("NodeId", 1);
    val group1 = domainValue("GroupId", 1); 
    val writAccessRight = AnyValue(DataTypeValue("AW", List()));

    // create admin
    s = i.executeAction(s, CallAction(createAdminInvocId, "createUser", List(AnyValue(true)))).get // create admin
    s = i.executeAction(s, LocalAction(createAdminInvocId, StartTransaction(createAdminTrasnactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createAdminInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createAdminInvocId, Return())).get
    println(s"admin created");
    
    // create user
    s = i.executeAction(s, CallAction(createUserInvocId, "createUser", List(AnyValue(false)))).get // create admin
    s = i.executeAction(s, LocalAction(createUserInvocId, StartTransaction(createUserTrasnactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createUserInvocId, NewId(2))).get
    s = i.executeAction(s, LocalAction(createUserInvocId, Return())).get
    println(s"user created");
    
    // create group
    s = i.executeAction(s, CallAction(createGroupInvocId, "createGroup", List())).get // create admin
    s = i.executeAction(s, LocalAction(createGroupInvocId, StartTransaction(createGroupTransactionId, Set()))).get
    s = i.executeAction(s, LocalAction(createGroupInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createGroupInvocId, Return())).get
    println("Group created")

    // assign user to group
    s = i.executeAction(s, CallAction(assignToGroupInvocId, "assignUserToGroup", List(admin1, group1, user1))).get
    s = i.executeAction(s, LocalAction(assignToGroupInvocId, StartTransaction(assignToGroupTransactionId, Set(createUserTrasnactionId, createAdminTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(assignToGroupInvocId, Return())).get
    println(s"user assigned to group");
    
    // create file
    s = i.executeAction(s, CallAction(createFileInvocId, "createFile", List(user1, group1))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, StartTransaction(createFileTrasnactionId, Set(assignToGroupTransactionId)))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, NewId(1))).get
    s = i.executeAction(s, LocalAction(createFileInvocId, Return())).get
    println(s"file created");

    // change owner perms
    s = i.executeAction(s, CallAction(changeOwnerPermsInvocId, "changeOwnerPermission", List(admin1, writAccessRight, file1))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId, StartTransaction(changeOwnerPermsTrasnactionId, Set(createFileTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(changeOwnerPermsInvocId, Return())).get
    println(s"owner perms changed");

    // read
    s = i.executeAction(s, CallAction(readFileInvocId, "readFile", List(user1, file1))).get
    s = i.executeAction(s, LocalAction(readFileInvocId, StartTransaction(readFileTrasnactionId, Set(changeOwnerPermsTrasnactionId)))).get
    s = i.executeAction(s, LocalAction(readFileInvocId, Return())).get
    println(s"done");

    try {
      i.checkInvariants(s)
    } catch {
        case iv: InvariantViolationException =>
            fail("Invariant-check should fail")
            println(iv)
            for (info <- iv.info)
                println(info)
    }

  }


}
