const std = @import("std");
const mem = std.mem;
const stdio = std.io.getStdOut().writer();

// TODOs:
// disallow passing the same arg twice
// add a way to allow `--` at the start of values?
// support positional arguments
// support comma-separate array values? --arrayArg=1,2,3 or --arrayArg 1,2,3


const ArgParseError = error{
  ArgsAllocError,
  NotEnoughArgs,
  UnexpectedTrailingEqual,
  // InvalidArgs,
  // MissingRequiredArgument,
  // ExpectedArgument,
  // CouldNotParseInteger,
  // CouldNotParseFloat,
  // UnexpectedArgument,
  // NotEnoughArrayArguments,
  // OutOfMemory,
};
const parserOptions = struct {
  longShortArgs: bool = false,
};


//#1

/// Make a type non-Optional
fn deOptioned(comptime T: type) type {
  const info = @typeInfo(T);
  return switch (info) {
    .Optional => info.Optional.child,
    else => T,
  };
}

/// make a type optional if it is not
fn optioned(comptime T: type) type {
  return switch (@typeInfo(T)) {
    .Optional => T,
    else => @Type(.{ .Optional = .{ .child = T } }),
  };
}

/// Decides weather the field type should be always optional,
/// sometimes optional or same as user specified
const optionalTypeType = enum { optional, nonOptional, auto, };

/// Decides weather the `struct { const outType: type = ...; fn parser(self: *Self) void {...} }`
/// will be converted to flatStruct i.e. `outType` or remain unchanged
const flatternType = enum { flatStruct, originalStruct, };

/// Flattene the struct according to the `optionalOption` and `flatOption` provided
fn flattenStruct(comptime T: type, comptime optionalOption: flatternType, comptime flatOption: flatternType) std.builtin.Type {
  const structInfo = @typeInfo(T).Struct;
  return .{
    .Struct = .{
      .layout = structInfo.layout,
      .backing_integer = structInfo.backing_integer,
      .decls = structInfo.decls,
      .is_tuple = structInfo.is_tuple,
      .fields = init: {
        var fieldsArr: [structInfo.fields.len]std.builtin.Type.StructField = undefined;
        inline for (structInfo.fields, 0..) |field, i| {
          fieldsArr[i] = .{
            .name = field.name,
            .type = initType: {
              switch (@typeInfo(field.type)) {
                .Struct => |structType| {
                  if (isNamedAlias(field.type)) {
                    switch (@typeInfo(field.type.parser)) {
                      .Fn => {
                        const outType = initOutType: {
                          if (flatOption == .originalStruct) { break :initOutType field.type; }
                          break :initOutType field.type.outType;
                        };
                        if (optionalOption == .nonOptional) break :initType deOptioned(outType);
                        if (optionalOption == .optional) break :initType optioned(outType);
                        break :initType outType;
                      },
                      else => { @compileLog("`parsed` was not a function, interpreting the struct as a saperator\n"); }
                    }
                    break :initType optioned(field.type);
                  }
                  break :initType optioned(@Type(flattenStruct(structType)));
                },
                // .Fn => |fnType| { break :initType fnType.return_type.?; },
                .Optional => |optionalType| {
                  if (optionalOption == .nonOptional) break :initType optionalType.child;
                  break :initType field.type;
                },
                else => {
                  if (optionalOption == .optional) break :initType optioned(field.type);
                  break :initType field.type;
                },
              }
            },
            .default_value = field.default_value,
            .is_comptime = field.is_comptime,
            .alignment = field.alignment,
          };
        }
        break :init &fieldsArr;
      },
    }
  };
}

//#2
/// Make a type non-optional

fn isShortArgType(name: []const u8) bool {
  if (name.len == 0) { @compileError("Zero length parameter encountered"); }
  else if (name.len == 1) { return true; }
  else if (name.len == 2) { return name[0] == '-' and name[1] != '-'; }
  return false;
}

fn isNamedAlias(comptime T: type) bool {
  if (@hasDecl(T, "parser") and @hasDecl(T, "parsed")) {
    return true;
  }
  return false;
}

// /// Parse process's command line arguments subject to the passed struct's format and parsing options
// pub fn parseArgsOpt(comptime T: type, options: ArgParseOptions) !T {
//   const args = ;
//   return parseArgsListOpt(T, args, options);
// }

/// The arg parser interface
fn Parser(comptime argumentType: type) type {
  const flatArgumentTypeInfo = flattenStruct(argumentType);
  const flatArgumentType = @Type(flatArgumentTypeInfo);
  const fieldAlias = struct { field: []const u8, alias: []const u8, };
  const aliasTypeEnum = enum { auto, short, long, saperator }; // auto is only `short` or `long`, never `saperator`

  // Count the length of each type of argument
  const aliasLengthType = struct {
    short: usize,
    long: usize,
    saperator: usize
  };
  const aliasesLengths: aliasLengthType = init: {
    var short: usize = 0;
    var long: usize = 0;
    var saperator: usize = 0;
    inline for (flatArgumentTypeInfo.Struct.fields) |field| {
      const info = @typeInfo(field.type);
      const incr = initIncr: {
        switch (info) {
          .Struct => {
            if (0) { break :initIncr -100; }
            else { saperator += 1; break :initIncr 0; }
          },
          else => { break :initIncr 1; },
        }
      };
      if (isShortArgType(field.name)) { short += incr; }
      else { long += incr; }
    }
    break: init .{ .short = short,.long = long, .saperator = saperator, };
  };

  // Initialize each type of alias
  const aliasType = struct{
    short: [aliasesLengths.short]fieldAlias,
    long: [aliasesLengths.long]fieldAlias,
    saperator: [aliasesLengths.saperator]fieldAlias,
  };
  const aliases: aliasType = init: {
    const combinedType = struct {
      length: aliasLengthType = .{ .short = 0, .long = 0, .saperator = 0, },
      alias: aliasType = .{ .short = undefined, .long = undefined, .saperator = undefined },

      const Self = @This();
      fn appendArg(self: *Self, arg: fieldAlias, argType: aliasTypeEnum) void {
        switch (argType) {
          .saperator => {
            self.alias.saperator[self.length.saperator] = arg;
            self.length.saperator += 1;
          },
          .short => {
            if (arg.alias[0] == '-') { self.alias.short[self.length.short] = arg; }
            else { self.alias.short[self.length.short] = .{ .field = arg.field, .alias = "-"++arg.alias, }; }
            self.length.short += 1;
          },
          .long => {
            if (arg.alias[0] == '-') { self.alias.long[self.length.long] = arg; }
            else { self.alias.long[self.length.long] = .{ .field = arg.field, .alias = "--"++arg.alias, }; }
            self.length.long += 1;
          },
          .auto => {
            if (isShortArgType(arg.alias)) { self.appendArg(arg, .small); }
            else { self.appendArg(arg, .long); }
          },
        }
      }
    };

    var returnVal: combinedType = .{};
    inline for (flatArgumentTypeInfo.Struct.fields) |field| {
      const name = field.name;
      const info = @typeInfo(field.type);
      switch (info) {
        .Struct => {
          returnVal.appendArg(.{ .alias = name, .field = name, }, .saperator);
        },
        else => {
          returnVal.appendArg(.{ .alias = name, .field = name, }, .auto);
        },
      }
    }
    break :init returnVal.alias;
  };

  return struct {
    allocator: std.mem.Allocator = std.heap.c_allocator,
    optionalArgs: ?[]const []const u8 = null,
    argIndex: usize = 1, // skip first arg (launch command)
    options: parserOptions = .{},

    var result: flatArgumentType = undefined;

    const Self = @This();
    // [ Small Args, Large Args, Saperators ] 
    // const x: []const fieldAlias = &[_]fieldAlias{.{.field = "name", .alias = "name",}};
    // const aliases: [3][]const fieldAlias = [_][]const fieldAlias{x,x,x};

    /// Parse arguments list
    fn parseArgs(self: *Self) ArgParseError!flatArgumentType {
      // make NonOptional args
      const args = self.optionalArgs orelse std.process.argsAlloc(self.allocator) catch { return error.ArgsAllocError; };
      self.optionalArgs = args;

      // There need to be arguments to be parsed!
      if (args.len < 1) { return error.NotEnoughArgs; }

      // process all args
      while (self.argIndex < args.len) : (self.argIndex += 1) {
        var arg = args[self.argIndex];
        const next: ?[]const u8 = initNext: {
          if (std.mem.indexOfScalar(u8, arg, '=')) |index| {
            if (index+1 < arg.len) {
              const val = arg[(index+1)..];
              arg = arg[0..index];
              break :initNext val;
            } else {
              return error.UnexpectedTrailingEqual;
            }
          }
          stdio.print("Present here:;\n", .{}) catch return error.ArgsAllocError;
          self.argIndex += 1;
          if (self.argIndex < args.len) {
            break :initNext args[self.argIndex];
          }
          break :initNext null;
        };
        if (arg[0] == '-') {
          if (arg[1] == '-') { // Long arg
            try self.parseArgument(.long, arg, next);
          } else { // Maybe Short arg
            if (self.options.longShortArgs) { try self.parseArgument(.long, arg, next); }
            try self.parseArgument(.short, arg, next);
          }
        } else {} // arg[0] == '-'
      }
      return result;
    }

    fn parseArgument(self: *Self, comptime argType: aliasTypeEnum, arg: []const u8, next: ?[]const u8) ArgParseError!void {
      const aliasesArray = init: {
        switch (argType) {
          .short => { break :init aliases.short; },
          .long => { break :init aliases.long; },
          .saperator => { break :init aliases.saperator; },
          .auto => { @compileError("Invalid type passed"); },
        }
      };
      inline for (aliasesArray) |name| {
        if (mem.eql(u8, arg, name.alias)) {
          const fieldType = @TypeOf(@field(result, name.field));
          switch (@typeInfo(fieldType)) {
            .Array => |arrayType| {
              if (arrayType.child == u8) {
                @field(result, name.field) = try self.parseValue(fieldType, next);
                return;
              }
            },
            // .Pointer => { unreachable; },
            else => { @field(result, name.field) = try self.parseValue(fieldType, next); },
          }
        }
      }
    }

    fn parseValue(self: *Self, comptime T: type, value: ?[]const u8) ArgParseError!T {
      _ = self;
      if (T == []const u8) return value orelse { return error.NotEnoughArgs; }; // strings
      switch (@typeInfo(T)) {
        .Bool => { return true; },
        .Int => { return std.fmt.parseInt(T, value, 10) catch { return error.CouldNotParseInteger; }; },
        .Float => { return std.fmt.parseFloat(T, value) catch { return error.CouldNotParseFloat; }; },
        .Struct => { @compileError("Unimplemented");  }, // TODO: Parse structs
        .Type => { @compileError("Unimplemented");  }, // TODO: Parse Types
        else => { @compileError("Unknown Type Encountered"); },
      }
    }

    // TODO: Figure out where to put help info
    fn printUsages(self: *Self) void { _ = self; }
  };
}


pub fn main() !void {
  const log = std.debug.print;
  const parserType = Parser(struct {
    name: []const u8,
  });
  var parser: parserType = .{};
  const parsed = try parser.parseArgs();
    log("{s}\n", .{parsed.name});
  // if (parsed.name) |name| {
  //   log("{s}\n", .{name});
  // }
}
