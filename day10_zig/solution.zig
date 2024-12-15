const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, {s}!\n", .{"world"});
    std.debug.print("Part 1:\n");
    solve_part_1();
    std.debug.print("Part 2:\n");
    solve_part_2();
}

fn read_input(filepath: *u8) [][]u8 {
    var grid

    // source: https://ziggit.dev/t/the-zig-way-to-read-a-file/4663/4
    var gpa = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true }){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) {
        std.log.err("Memory leak", .{});
    };

    const file = std.fs.cwd().openFile(filepath, .{}) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return;
    };
    defer file.close();

    while (file.reader().readUntilDelimiter(allocator, '\n') catch |err| {
        std.log.err("Failed to read line: {s}", .{@errorName(err)});
        return;
    }) |line| {
        defer allocator.free(line);

    }
}

fn solve_part_1() !void {
    const score: u32 = 0;

    std.debug.print("{}\n", .{score});
}

fn solve_part_2() !void {
    const score: u32 = 0;

    std.debug.print("{}\n", .{score});
}
